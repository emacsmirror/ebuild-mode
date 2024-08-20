;;; ebuild-mode.el --- edit ebuild and eclass files  -*-lexical-binding:t-*-

;; Copyright 2006-2024 Gentoo Authors

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Version: 1.73
;; Keywords: languages, processes

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'sh-script)
(require 'font-lock)
(require 'easymenu)
(require 'skeleton)


;;; Variables.

(defgroup ebuild nil
  "Major mode for Gentoo .ebuild and .eclass files."
  :group 'languages)

(defcustom ebuild-mode-full-name
  user-full-name
  "Full name of user, to be used in ebuild repositories."
  :type 'string
  :group 'ebuild)

(defcustom ebuild-mode-mail-address
  user-mail-address
  "E-mail address of user, to be used in ebuild repositories."
  :type 'string
  :group 'ebuild)

(defcustom ebuild-mode-portdir
  (cond ((file-directory-p "/var/db/repos/gentoo") "/var/db/repos/gentoo")
	((file-directory-p "/usr/portage") "/usr/portage")
	(t "/var/db/repos/gentoo"))
  "Location of the ebuild repository."
  :type 'string
  :group 'ebuild)

(defcustom ebuild-mode-portage-tmpdir
  (cond ((file-directory-p "/var/tmp/portage") "/var/tmp/portage")
	((file-directory-p "/tmp/portage") "/tmp/portage")
	(t "/var/tmp/portage"))
  "Location Portage will use for compilations and temporary storage."
  :type 'string
  :group 'ebuild)

(defcustom ebuild-mode-eapi-list
  '("6" "7" "8")
  "List of supported EAPIs.
The most recent EAPI must be listed last."
  :type '(repeat string)
  :group 'ebuild)

(defcustom ebuild-mode-fix-whitespace 'ebuild
  "If non-nil, fix whitespace before writing a file.
Namely, delete trailing whitespace and tabify whitespace at beginning
of lines.
If the value is `ebuild', fixes for tabs and newlines will only
be applied to ebuilds but not to eclasses."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)
		 (const :tag "Ebuilds only" ebuild))
  :group 'ebuild)

(defcustom ebuild-mode-update-copyright t
  "Whether to update the copyright notice before writing a file.
If the value is a list of booleans, then its first and second element
control updating of the year and the author, respectively.
If any other non-nil value, update both.
If nil, don't update."
  :type '(choice boolean
		 (list (boolean :tag "Year")
		       (boolean :tag "Author")))
  :group 'ebuild)

(defcustom ebuild-mode-delete-cvs-line nil
  "If non-nil, delete any CVS $Id$ or $Header$ line before writing a file."
  :type 'boolean
  :group 'ebuild)

(defcustom ebuild-mode-xml-indent-tabs nil
  "If non-nil, use tab characters for indenting of XML.
If nil, use two spaces."
  :type 'boolean
  :group 'ebuild)

(defcustom ebuild-mode-process-environment
  (unless (fboundp 'ansi-color-compilation-filter)
    '("NO_COLOR=1"))
  "List of additional environment variables for subprocesses.
Each element should be a string of the form NAME=VALUE.  This will
be prepended to `process-environment' when calling a subprocess."
  :type '(repeat string)
  :group 'ebuild)

(defun ebuild-mode-arch-lessp (a b)
  "Predicate function for comparison of architecture keywords.
Returns non-nil if A is less than B by Gentoo keyword ordering."
  (let* ((tail (make-list 2 ""))
	 (as (nconc (split-string (or (car-safe a) a) "-") tail))
	 (bs (nconc (split-string (or (car-safe b) b) "-") tail)))
    ;; two-part keywords: first compare the OS (after hyphen, if any),
    ;; then the architecture (before hyphen)
    (if (string-equal (cadr as) (cadr bs))
	(string-lessp (car as) (car bs))
      (string-lessp (cadr as) (cadr bs)))))

(defvar ebuild-mode-arch-list
  (or
   (condition-case nil
       (with-temp-buffer
	 (insert-file-contents-literally
	  (concat ebuild-mode-portdir "/profiles/arch.list"))
	 (while (re-search-forward "#.*$" nil t)
	   (replace-match ""))
	 (sort (split-string (buffer-string)) #'ebuild-mode-arch-lessp))
     (file-error nil))
   ;; could not read architectures from repository, so fall back to default
   '("alpha" "amd64" "arm" "arm64" "hppa" "ia64" "loong" "m68k" "mips"
     "ppc" "ppc64" "riscv" "s390" "sparc" "x86"))
  "List of architectures.")

(defvar ebuild-mode-arch-stable-list
  (or
   ;; try to read arches.desc (GLEP 72) first, then profiles.desc
   (condition-case nil
       (with-temp-buffer
	 (insert-file-contents-literally
	  (concat ebuild-mode-portdir "/profiles/arches.desc"))
	 (let (arch archs)
	   (while (re-search-forward
		   "^[ \t]*\\([^ \t\n#]+\\)[ \t]+\\(stable\\|transitional\\)\\>"
		   nil t)
	     (setq arch (match-string 1))
	     (and (not (member arch archs))
		  (member arch ebuild-mode-arch-list)
		  (setq archs (cons arch archs))))
	   (sort archs #'ebuild-mode-arch-lessp)))
     (file-error nil))
   (condition-case nil
       (with-temp-buffer
	 (insert-file-contents-literally
	  (concat ebuild-mode-portdir "/profiles/profiles.desc"))
	 (let (arch archs)
	   (while (re-search-forward
		   "^[ \t]*\\([^ \t\n#]+\\)[ \t]+[^ \t\n#]+[ \t]+stable\\>"
		   nil t)
	     (setq arch (match-string 1))
	     (and (not (member arch archs))
		  (member arch ebuild-mode-arch-list)
		  (setq archs (cons arch archs))))
	   (sort archs #'ebuild-mode-arch-lessp)))
     (file-error nil))
   ;; fall back to list of all architectures
   ebuild-mode-arch-list)
  "List of stable architectures.")

(defvar ebuild-mode-arch-regexp
  "^[ \t]*KEYWORDS=[\"']\\([^\"]*\\)[\"'][ \t]*$")

(defvar ebuild-mode-copyright-regexp
  "^#[ \t]*Copyright[ \t]+\\([1-9][0-9]+\\)\\(?:-\\([1-9][0-9]+\\)\\)?\
[ \t]+\\(.*\\<\\(?:Gentoo Authors\\|Gentoo Foundation\\)\\>.*\\)")

(defvar ebuild-mode-cvs-header-regexp
  "^#[ \t]*\\$\\(Id\\|Header\\)\\(: .*\\)?\\$[ \t]*$")

(defvar ebuild-mode-protocols-homepage
  '("http://" "https://" "ftp://"))

(defvar ebuild-mode-protocols-src_uri
  '("http://" "https://" "ftp://" "mirror://"))

(defvar ebuild-mode-licenses
  (condition-case nil
      (directory-files (concat ebuild-mode-portdir "/licenses")
		       nil "\\`[^.]")
    (file-error nil))
  "List of licenses, determined from the ebuild repository.")

(defvar ebuild-mode-eclasses
  (condition-case nil
      (mapcar
       (lambda (x) (substring x 0 (string-match "\\.eclass\\'" x)))
       (directory-files (concat ebuild-mode-portdir "/eclass")
			nil "\\.eclass\\'"))
    (file-error nil))
  "List of eclasses, determined from the ebuild repository.")

(defvar ebuild-mode-restrict-list
  '("binchecks" "bindist" "fetch" "installsources" "mirror"
    "network-sandbox" "preserve-libs" "primaryuri" "splitdebug"
    "strip" "test" "userpriv")
  "List of RESTRICT features.")

(defvar ebuild-mode-use-flags
  (condition-case nil
      (with-temp-buffer
	(insert-file-contents-literally
	 (concat ebuild-mode-portdir "/profiles/use.desc"))
	(while (re-search-forward "[ \t#].*$" nil t)
	  (replace-match ""))
	(split-string (buffer-string)))
    (file-error nil))
  "List of USE flags.")

(defvar ebuild-mode-action-alist
  '(("unstable" . "~")
    ("stable" . "")
    ("mask" . "-")
    ("drop" . nil)))

(defvar ebuild-commands-list
  '("clean" "cleanrm" "compile" "config" "configure" "depend" "digest"
    "fetch" "fetchall" "help" "info" "install" "instprep" "manifest"
    "merge" "nofetch" "package" "postinst" "postrm" "preinst" "prepare"
    "prerm" "pretend" "qmerge" "rpm" "setup" "test" "unmerge" "unpack")
  "List of ebuild sub-commands.")

(defvar ebuild-mode-pkgdev-commands
  '("commit" "manifest" "mask" "push" "showkw")
  "List of pkgdev sub-commands.")

(defvar ebuild-mode-pkgcheck-commands
  '("cache" "ci" "replay" "scan" "show")
  "List of pkgcheck sub-commands.")

;; suppress byte-compiler warning in XEmacs
(defvar ebuild-mode-menu)


;;; Compatibility code.

(eval-when-compile
  (unless (fboundp 'static-if)
    (defmacro static-if (cond then &rest else) ; from APEL
      "Like `if', but evaluate COND at compile time."
      (if (eval cond)
	  then
	`(progn ,@else)))
    ))

(defun ebuild-mode-time-string (format-string &optional time)
  "Use FORMAT-STRING to format the time value TIME.
Calls `format-time-string' (which see) for the UTC time zone.
Compatibility function for XEmacs."
  (static-if (and (featurep 'xemacs)
		  (not (function-allows-args #'format-time-string 3)))
      ;; format-time-string in older XEmacs versions can take only two
      ;; arguments. Version 21.5.35 still doesn't support a time zone
      ;; as third argument, but accepts non-nil to mean Universal Time.
      (let ((process-environment (copy-sequence process-environment))
	    (tz (getenv "TZ")))
	(unwind-protect
	    (progn
	      (setenv "TZ" "UTC")
	      (format-time-string format-string time))
	  ;; This is needed because setenv handles TZ specially.
	  ;; So, restoring the environment is not enough.
	  (setenv "TZ" tz)))
    (format-time-string format-string time t)))

;;; Font-lock.

(eval-and-compile
  (defun ebuild-mode-collect-and-split (src &optional limit)
    "For alist SRC, collect elements with equal cdr and concat their cars.
Optional argument LIMIT specifies the maximum length for the car
of the elements."
    (let (dst e)
      (dolist (c src dst)
	(setq e (rassoc (cdr c) dst))
	(cond
	 ((and e (or (not limit)
		     (<= (+ (length (car e)) (length (car c))) limit)))
	  ;; cdrs of new element C and previous element E are equal,
	  ;; and their combined length is below LIMIT => append to E
	  (setcar e (append (car e) (car c))))
	 ((or (not limit)
	      (<= (length (car c)) limit))
	  ;; new element C is small enough => push to DST
	  (push (cons (car c) (cdr c)) dst))
	 (t
	  ;; otherwise, split the new element into chunks of length LIMIT
	  (let ((cc (car c)))
	    (while cc
	      (push (cons (last cc limit) (cdr c)) dst)
	      (setq cc (butlast cc limit)))))))))
  )

(eval-when-compile
  (require 'ebuild-mode-keywords))

(defvar ebuild-mode-font-lock-keywords
  (eval-when-compile
    (mapcar
     (lambda (x)
       (cons (concat (or (nth 2 x) "\\<")
		     (regexp-opt (car x) t)
		     (or (nth 3 x) "\\>"))
	     (cadr x)))
     ;; Emacs has a limit of 32 kbyte for the size of regular
     ;; expressions. Unfortunately, this is a hard limit in Emacs'
     ;; C code, MAX_BUF_SIZE in regex.c, which cannot be increased.
     ;; Therefore, split the list into several parts with at most
     ;; 1000 keywords; this appears to keep the regexp size below
     ;; the limit.
     (ebuild-mode-collect-and-split
      (list ebuild-mode-keywords-EAPI
	    ebuild-mode-keywords-0
	    ebuild-mode-keywords-functions
	    ebuild-mode-keywords-sandbox
	    ebuild-mode-keywords-eapi-deprecated
	    ebuild-mode-keywords-warn
	    ebuild-mode-keywords-eclassdoc
	    ebuild-mode-keywords-eclassdoc-warn
	    ebuild-mode-keywords-eclass)
      1000))))

;;; Mode definitions.

(defun ebuild-mode-tabify ()
  "Tabify whitespace at beginning of lines."
  ;; We cannot use the following since XEmacs doesn't support tabify-regexp.
  ;;(let ((tabify-regexp "^\t* [ \t]+"))
  ;;  (tabify (point-min) (point-max)))
  (let ((tabify-regexp "^\t* [ \t]+")
	(indent-tabs-mode t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward tabify-regexp nil t)
	(let ((end-col (current-column))
	      (beg-col (save-excursion (goto-char (match-beginning 0))
				       (skip-chars-forward "\t")
				       (current-column))))
	  (if (= (/ end-col tab-width) (/ beg-col tab-width))
	      nil
	    (delete-region (match-beginning 0) (point))
	    (indent-to end-col)))))))

(defun ebuild-mode-delete-trailing-whitespace ()
  "Delete all the trailing spaces and tabs across the current buffer."
  ;; Simple non-interactive version of delete-trailing-whitespace
  ;; which doesn't exist in XEmacs
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))))

(defun ebuild-mode-squash-empty-lines ()
  "Replace multiple consecutive empty lines by a single one."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\n+$" nil t)
      (replace-match ""))))

(defun ebuild-mode-update-copyright ()
  "Update the copyright notice in the ebuild's header."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
	  (update-year (or (nlistp ebuild-mode-update-copyright)
			   (nth 0 ebuild-mode-update-copyright)))
	  (update-author (or (nlistp ebuild-mode-update-copyright)
			     (nth 1 ebuild-mode-update-copyright))))
      (when (re-search-forward ebuild-mode-copyright-regexp 400 t)
	(if update-year
	    (let* ((y1 (string-to-number (match-string 1)))
		   (y2 (and (match-string 2)
			    (string-to-number (match-string 2))))
		   (year (save-match-data (ebuild-mode-time-string "%Y")))
		   (y (string-to-number year)))
	      (if y2
		  ;; Update range of years
		  (cond ((or (> 1999 y1) (>= y1 y2) (> y2 y))
			 ;; XEmacs wants 'warning instead of :warning,
			 ;; but nil always works (and defaults to :warning)
			 (lwarn 'ebuild nil
				"Suspicious range of copyright years: %d-%d"
				y1 y2))
			((/= y2 y)
			 (replace-match year t t nil 2)))
		;; Update single year and convert to range if necessary
		(cond ((or (> 1999 y1) (> y1 y))
		       (lwarn 'ebuild nil "Suspicious copyright year: %d" y1))
		      ((/= y1 y)
		       (replace-match (concat "\\1-" year) t nil nil 1))))))
	(if update-author
	    ;; Update default author in copyright notice
	    (if (string-equal (match-string 3) "Gentoo Foundation")
		(replace-match "Gentoo Authors" t t nil 3)))))))

(defun ebuild-mode-delete-cvs-line ()
  "Remove a CVS $Id$ or $Header$ line from the ebuild's header."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (if (re-search-forward ebuild-mode-cvs-header-regexp 400 t)
	  (delete-region (match-beginning 0) (1+ (point)))))))

;;;###autoload
(define-derived-mode ebuild-mode sh-mode "Ebuild"
  "Major mode for Gentoo .ebuild files."
  ;; Always enable ebuild-repo-mode, even if the ebuild is edited
  ;; outside an ebuild repository
  (ebuild-repo-mode 1)
  (sh-set-shell "bash")
  (static-if (featurep 'xemacs)
      (easy-menu-add ebuild-mode-menu))
  (setq fill-column 72)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(defun ebuild-mode-add-font-lock ()
  "Add `ebuild-mode' font-lock keywords for the current buffer."
  (font-lock-add-keywords nil ebuild-mode-font-lock-keywords))

(add-hook 'ebuild-mode-hook #'ebuild-mode-add-font-lock)

;;;###autoload
(define-derived-mode ebuild-eclass-mode ebuild-mode "Eclass"
  "Major mode for Gentoo .eclass files."
  ;; Eclass documentation uses two spaces after sentence ends
  (set (make-local-variable 'sentence-end-double-space) t)
  ;; Don't rewrap paragraphs into a preceding eclassdoc token
  (set (make-local-variable 'paragraph-separate)
       (concat paragraph-start "\\|^# @")))

;;; Run ebuild command.

(defvar ebuild-mode-ebuild-history nil)
(defvar ebuild-mode-pkgdev-history nil)
(defvar ebuild-mode-pkgcheck-history nil)

(defvar ebuild-log-buffer-mode 'ebuild-compilation-mode
  "Major mode for the log buffer of `ebuild-run-command'.
If nil, `compilation-mode' will be used.")

(defun ebuild-run-command (command)
  "Run ebuild COMMAND, with output to a compilation buffer."
  (interactive
   (list
    (completing-read "Run ebuild command(s): "
		     #'ebuild-mode-ebuild-cmd-complete
		     nil nil nil 'ebuild-mode-ebuild-history)))
  (or buffer-file-name
      (error "No file for this buffer"))
  (let* ((file (file-relative-name buffer-file-name))
	 (shell-command (format "ebuild %s %s" file command))
	 (process-environment (append ebuild-mode-process-environment
				      process-environment))
	 ;;(compilation-mode-hook (lambda () (setq truncate-lines t)))
	 (compilation-buffer-name-function (lambda (_mode) "*ebuild*")))
    (static-if (featurep 'xemacs)
	(compile shell-command)
      (compile shell-command ebuild-log-buffer-mode))))

;; Define functions for all ebuild subcommands
(dolist (command ebuild-commands-list)
  (defalias (intern (concat "ebuild-run-command-" command))
    ;; Backquote for XEmacs compatibility (no lexical binding).
    ;; Also, defalias in 21.4 accepts only two args, so the docstring
    ;; must be in the lambda form.
    `(lambda (&optional clean)
       ,(format
	 "Run ebuild \"%s\" command, with output to a compilation buffer.
With prefix argument CLEAN, run the \"clean\" command first"
	 command)
       (interactive "P")
       (ebuild-run-command (concat (if clean "clean ") ,command)))))

(define-derived-mode ebuild-compilation-mode compilation-mode "Compilation"
  "Like `compilation-mode' but with color support.
Translates ANSI SGR control sequences into text properties (if the
Emacs version supports it).  Variable `ansi-color-for-compilation-mode'
must be non-nil for this to have any effect."
  (if (fboundp 'ansi-color-compilation-filter)
      (add-hook 'compilation-filter-hook
		#'ansi-color-compilation-filter nil t)))

(defun ebuild-mode-get-completion-function (mode)
  "Get completion function for completion mode MODE."
  (cond ((null mode) #'try-completion)
	((eq mode t) #'all-completions)
	((eq mode 'lambda)
	 (static-if (fboundp 'test-completion)
	     #'test-completion
	   ;; XEmacs 21.4 doesn't have test-completion
	   (lambda (&rest args)
	     (eq (apply #'try-completion args) t))))
	(t #'ignore)))

(defun ebuild-mode-ebuild-cmd-complete (s predicate mode)
  "Completion function for ebuild command.
To be used as second argument of `completing-read', which see for
an explanation of arguments S, PREDICATE and MODE."
  (string-match "^\\(.*\\s-\\)?\\(.*\\)$" s)
  (if (eq (car-safe mode) 'boundaries)
      (cons 'boundaries
	    (cons (match-beginning 2)
		  (string-match "\\s-" (cdr mode))))
    (let* ((s1 (match-string 1 s))
	   (s2 (match-string 2 s))
	   (c2 (funcall	(ebuild-mode-get-completion-function mode)
			s2
			(mapcar (lambda (x) (concat x " "))
				ebuild-commands-list)
			predicate)))
      (if (stringp c2) (concat s1 c2) c2))))

(defun ebuild-mode-command-complete (s predicate mode)
  "Completion function for pkgdev and pkgcheck commands.
To be used as second argument of `completing-read', which see for
an explanation of arguments S, PREDICATE and MODE."
  (string-match "^\\(.*\\s-\\)?\\(.*\\)$" s)
  (if (eq (car-safe mode) 'boundaries)
      (cons 'boundaries
	    (cons (match-beginning 2)
		  (string-match "\\s-" (cdr mode))))
    (let* ((s1 (match-string 1 s))
	   (s2 (match-string 2 s))
	   (c2 (funcall
		(ebuild-mode-get-completion-function mode)
		s2
		(mapcar (lambda (x) (concat x " "))
			(cond ((not (stringp s1))
			       '("pkgdev" "pkgcheck"))
			      ((string-match "\\<pkgdev\\s-+$" s1)
			       ebuild-mode-pkgdev-commands)
			      ((string-match "\\<pkgcheck\\s-+$" s1)
			       ebuild-mode-pkgcheck-commands)
			      (t (list s2))))
		predicate)))
      (if (stringp c2) (concat s1 c2) c2))))

(defun ebuild-mode-run-pkgdev (command)
  "Run pkgdev COMMAND with output to a compilation buffer.
Like `compile', but with autocompletion for pkgdev."
  (interactive
   (list (completing-read "Run pkgdev command: "
			  #'ebuild-mode-command-complete
			  nil nil "pkgdev "
			  'ebuild-mode-pkgdev-history)))
  (let ((process-environment (append ebuild-mode-process-environment
				     process-environment))
	(compilation-buffer-name-function (lambda (_mode) "*pkgdev*")))
    (static-if (featurep 'xemacs)
	(compile command)
      (compile command ebuild-log-buffer-mode))))

(defun ebuild-mode-run-pkgcheck (command)
  "Run pkgcheck COMMAND with output to a compilation buffer.
Like `compile', but with autocompletion for pkgcheck."
  (interactive
   (list (completing-read "Run pkgcheck command: "
			  #'ebuild-mode-command-complete
			  nil nil "pkgcheck "
			  'ebuild-mode-pkgcheck-history)))
  (let ((process-environment (append ebuild-mode-process-environment
				     process-environment))
	(compilation-buffer-name-function (lambda (_mode) "*pkgcheck*")))
    (static-if (featurep 'xemacs)
	(compile command)
      (compile command ebuild-log-buffer-mode))))

(defun ebuild-mode-get-builddir ()
  "Get Portage's build directory for the ebuild in this buffer."
  (unless buffer-file-name
    (error "No file for this buffer"))
  (let* ((pkgdir (directory-file-name (file-name-directory buffer-file-name)))
	 (catdir (directory-file-name (file-name-directory pkgdir)))
	 (category (file-name-nondirectory catdir))
	 (pn (file-name-nondirectory pkgdir))
	 (basename (file-name-nondirectory buffer-file-name))
	 (pf (file-name-sans-extension basename)))
    ;; sanity check
    (unless (string-match "\\.ebuild\\'" (file-name-sans-versions basename))
      (error "Filename \"%s\" does not end in \".ebuild\"" basename))
    (unless (and (file-exists-p
		  (expand-file-name "../profiles/repo_name" catdir))
		 (string-match (concat "\\`" (regexp-quote pn) "-") pf))
      (error "This does not look like an ebuild repository"))
    (let ((builddir (concat (file-name-as-directory ebuild-mode-portage-tmpdir)
			    category "/" pf)))
      (unless (file-directory-p builddir)
	(error "Package build directory \"%s\" does not exist" builddir))
      builddir)))

(defun ebuild-mode-find-workdir (&optional other-window)
  "Visit the working directory (WORKDIR) for the ebuild in this buffer.
With prefix argument OTHER-WINDOW, visit the directory in another window."
  (interactive "P")
  (let ((workdir (concat (ebuild-mode-get-builddir) "/work"))
	(find-file-run-dired t))
    (unless (file-directory-p workdir)
      (error "WORKDIR=\"%s\" does not exist" workdir))
    (if other-window
	(find-file-other-window workdir)
      (find-file workdir))))

(defun ebuild-mode-find-image-dir (&optional other-window)
  "Visit the image directory (D) for the ebuild in this buffer.
With prefix argument OTHER-WINDOW, visit the directory in another window."
  (interactive "P")
  (let ((image (concat (ebuild-mode-get-builddir) "/image"))
	(find-file-run-dired t))
    (unless (file-directory-p image)
      (error "D=\"%s\" does not exist" image))
    (if other-window
	(find-file-other-window image)
      (find-file image))))

(defun ebuild-mode-unescape-string (s &optional ansi-c)
  "Convert string S by expanding backslash escape sequences.
With optional argument ANSI-C, expand a string with ANSI C escape
sequences, instead of a simple double-quoted string.

This function supports only escape sequences that can occur in
the output of the \"declare -p\" Bash command."
  (let ((case-fold-search nil)
	(decode-re (if ansi-c
		       "\\\\\\([abtnvfreE\\'\"?]\\|[0-7]\\{1,3\\}\\)"
		     "\\\\\\([$`\"\\\n]\\)"))
	(decode-alist '((?a . ?\a) (?b . ?\b) (?t . ?\t) (?n . ?\n) (?v . ?\v)
			(?f . ?\f) (?r . ?\r) (?e . ?\e) (?E . ?\e)))
	i)
    (while (setq i (string-match decode-re s i))
      (let* ((m (match-string 1 s))
	     (c (aref m 0))
	     (byte (cond ((and (>= c ?0) (< c ?8))
			  ;; no string-to-number with base in XEmacs 21.4
			  (let ((n 0))
			    (dotimes (j (length m))
			      (setq n (+ (* n 8) (- (aref m j) ?0))))
			    (logand n #xff)))
			 ((cdr (assq c decode-alist)))
			 (t c))))
	(setq s (replace-match
		 (static-if (fboundp 'byte-to-string)
		     (byte-to-string byte)
		   (char-to-string byte))
		 nil t s))
	(setq i (1+ i))))
    s))

(defun ebuild-mode-find-s (&optional other-window)
  "Visit the temporary build directory (S) for the ebuild in this buffer.
With prefix argument OTHER-WINDOW, visit the directory in another window."
  (interactive "P")
  (let ((builddir (ebuild-mode-get-builddir))
	s)
    (condition-case nil
	(with-temp-buffer
	  (insert-file-contents-literally
	   (concat builddir "/temp/environment"))
	  (re-search-forward
	   "^declare -\\S-* S=\\(\"\\|\\$'\\)\\(.*\\)[\"']$")
	  (setq s (ebuild-mode-unescape-string
		   (match-string 2)
		   (string-equal (match-string 1) "$'"))))
      (file-error (error "Failed to read environment file"))
      (search-failed (error "Could not find S in the environment file")))
    (ignore-errors
      (setq s (decode-coding-string s 'utf-8-unix)))
    (unless (file-directory-p s)
      (error "Directory S=\"%s\" does not exist" s))
    ;; sanity check, S should be WORKDIR or a subdir of it
    ;; XEmacs does not have file-in-directory-p
    (let* ((workdir (concat builddir "/work"))
	   (wd (file-name-as-directory workdir))
	   (sd (file-name-as-directory s))
	   (find-file-run-dired t))
      (unless (and (>= (length sd) (length wd))
		   (string-equal (substring sd 0 (length wd)) wd))
	(error "S=\"%s\" is outside WORKDIR=\"%s\"" s workdir))
      (if other-window
	  (find-file-other-window s)
	(find-file s)))))

(defun ebuild-mode-find-build-log (&optional other-window)
  "Visit the build log for the ebuild in this buffer.
With prefix argument OTHER-WINDOW, visit the directory in another window."
  (interactive "P")
  (let ((build-log (concat (ebuild-mode-get-builddir) "/temp/build.log")))
    (unless (file-readable-p build-log)
      (error "Cannot read file \"%s\"" build-log))
    (if other-window
	(find-file-other-window build-log)
      (find-file build-log))
    ;; decode ANSI SGR control sequences if possible (tty-format.el)
    (and (assq 'ansi-colors format-alist)
	 (save-excursion
	   (goto-char (point-min))
	   (re-search-forward "\e\\[[0-9;]*m" nil t))
	 (format-decode-buffer 'ansi-colors))))

;;; Modify package keywords.
;; This is basically a reimplementation of "ekeyword" in Emacs Lisp.

(defun ebuild-mode-get-keywords (&optional noerror)
  "Get architecture keywords from an ebuild's KEYWORDS line.
Signal an error if this is not possible, or return nil if the
optional argument NOERROR is non-nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (cond
       ((not (re-search-forward ebuild-mode-arch-regexp nil t))
	(unless noerror (error "No KEYWORDS assignment found")))
       ((re-search-forward ebuild-mode-arch-regexp nil t) ; second search
	(unless noerror (error "More than one KEYWORDS assignment found")))
       (t
	(mapcar (lambda (s)
		  (string-match "^\\([-~]?\\)\\(.*\\)" s)
		  (cons (match-string 2 s) (match-string 1 s)))
		(split-string
		 ;;(match-string-no-properties 1) ; not in XEmacs 21.4
		 (buffer-substring-no-properties (match-beginning 1)
						 (match-end 1)))))))))

(defun ebuild-mode-put-keywords (kw &optional noerror)
  "Replace the ebuild's KEYWORDS by those given in the string KW.
Signal an error if this is not possible, or return nil if the
optional second argument NOERROR is non-nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
	  (kw-string (mapconcat
		      (lambda (e) (concat (cdr e) (car e))) kw " ")))
      (cond
       ((not (re-search-forward ebuild-mode-arch-regexp nil t))
	(unless noerror (error "No KEYWORDS assignment found")))
       ((re-search-forward ebuild-mode-arch-regexp nil t)
	(unless noerror (error "More than one KEYWORDS assignment found")))
       (t
	(unless (string-equal kw-string (match-string 1))
	  (replace-match kw-string t t nil 1)))))))

(defun ebuild-mode-modify-keywords (kw)
  "Set keywords.  KW is an alist of architectures and leaders."
  (let ((keywords (ebuild-mode-get-keywords)))
    (dolist (k kw)
      (let* ((arch (car k))
	     (leader (cdr k))
	     (old-k (assoc arch keywords)))
	(cond
	 ;; remove keywords
	 ((null leader)
	  (setq keywords (and (not (string-equal arch "all"))
			      (delq old-k keywords))))
	 ;; modify all non-masked keywords in the list
	 ((string-equal arch "all")
	  (dolist (e keywords)
	    (and (or (equal (cdr e) "")
		     (equal (cdr e) "~"))
		 (member (car e)
			 (if (equal leader "")
			     ebuild-mode-arch-stable-list
			   ebuild-mode-arch-list))
		 (setcdr e leader))))
	 ;; modify keyword
	 (old-k (setcdr old-k leader))
	 ;; add keyword
	 (t (setq keywords (cons k keywords))))))
    (ebuild-mode-put-keywords
     (sort keywords #'ebuild-mode-arch-lessp))))

(defun ebuild-mode-keyword (action arch)
  "Keyword manipulation.
ACTION must be an action from `ebuild-mode-action-alist', ARCH an
architecture from `ebuild-mode-arch-list'."
  (interactive
   (list
    (cdr (assoc (completing-read "Action: " ebuild-mode-action-alist
				 nil t nil nil "unstable")
		ebuild-mode-action-alist))
    (completing-read "Architecture: "
		     (mapcar #'list
			     (append '("all" "*") ebuild-mode-arch-list))
		     nil t)))
  (ebuild-mode-modify-keywords (list (cons arch action))))

(defun ebuild-mode-ekeyword-complete (s predicate mode)
  "Completion function, to be used as second argument of `completing-read'.
Return common substring of all completions of S for given PREDICATE.
MODE can be nil, t, or `lambda'.  See documentation of `try-completion'
and `all-completions' for details."
  (string-match "^\\(.*\\s-\\)?\\(.*\\)$" s)
  (if (eq (car-safe mode) 'boundaries) ; GNU Emacs 23
      (cons 'boundaries
	    (cons (match-beginning 2)
		  (string-match "\\s-" (cdr mode))))
    (let* ((s1 (match-string 1 s))
	   (s2 (match-string 2 s))
	   (c2 (funcall
		(ebuild-mode-get-completion-function mode)
		s2
		(mapcar #'list
			(if (string-equal s2 "")
			    '("" "~" "-" "^")
			  (string-match "^[-^~]?" s2)
			  (let ((s3 (match-string 0 s2)))
			    (mapcar (lambda (x) (concat s3 x " "))
				    (append '("all")
					    (and (member s3 '("-" "^"))
						 '("*"))
					    (if (equal s3 "")
						ebuild-mode-arch-stable-list
					      ebuild-mode-arch-list))))))
		predicate)))
      (if (stringp c2) (concat s1 c2) c2))))

(defun ebuild-mode-ekeyword (keywords)
  "Keyword manipulation.  Accepts the same input format as ekeyword.
KEYWORDS is a whitespace separated string containing the keywords
that shall be manipulated."
  (interactive
   (list (completing-read "Keywords: " #'ebuild-mode-ekeyword-complete)))
  (ebuild-mode-modify-keywords
   (mapcar (lambda (s)
	     (string-match "^\\([-^~]?\\)\\(.*\\)" s)
	     (cons (match-string 2 s)
		   (and (not (equal (match-string 1 s) "^"))
			(match-string 1 s))))
	   (split-string keywords))))

(defun ebuild-mode-all-keywords-unstable ()
  "Mark all keywords as unstable."
  (interactive)
  (ebuild-mode-modify-keywords '(("all" . "~"))))

;;; Skeleton support.

(define-skeleton ebuild-mode-insert-skeleton
  "Insert a statement skeleton for a new ebuild."
  nil
  ;; standard header
  "# Copyright " (ebuild-mode-time-string "%Y") " Gentoo Authors\n"
  "# Distributed under the terms of the GNU General Public License v2\n"
  "\n"
  "EAPI="
  (completing-read
   "EAPI: " (mapcar #'list ebuild-mode-eapi-list)
   nil nil (car (last ebuild-mode-eapi-list))) ; default to most recent EAPI
  "\n"
  "\n"
  ;; inherited eclasses
  "inherit "
  ((completing-read "Eclass (null string to terminate): "
		    (mapcar #'list ebuild-mode-eclasses))
   str & " ")
  & -1 & "\n\n" | -8
  ;; first variables block
  "DESCRIPTION=\"" (skeleton-read "Description: ") "\"\n"
  "HOMEPAGE=\""
  (completing-read "Homepage: "
		   (mapcar #'list ebuild-mode-protocols-homepage))
  "\"\n"
  "SRC_URI=\""
  (completing-read "Source URI: "
		   (mapcar #'list ebuild-mode-protocols-src_uri))
  "\"\n"
  "S=\""
  (completing-read "S (null string for default): "
		   '(("${WORKDIR}") ("${WORKDIR}/${PN}")))
  & "\"\n" | -3
  "\n"
  ;; second variables block
  "LICENSE=\""
  ((completing-read "License (null string to terminate): "
		    (mapcar #'list ebuild-mode-licenses))
   str & " ")
  & -1 "\"\n"
  "SLOT=\"0\"\n"
  "KEYWORDS=\""
  ((completing-read
    "Keyword (null string to terminate): "
    (nconc
     (mapcar (lambda (x) (list (concat "~" x))) ebuild-mode-arch-list)
     (mapcar #'list ebuild-mode-arch-stable-list)))
   str & " ")
  & -1 "\"\n"
  "IUSE=\""
  ((completing-read "USE flag (null string to terminate): "
		    (mapcar #'list ebuild-mode-use-flags))
   str & " ")
  & -1 & "\"\n" | -6
  "RESTRICT=\""
  ((completing-read "RESTRICT (null string to terminate): "
		    (mapcar #'list ebuild-mode-restrict-list))
   str & " ")
  & -1 & "\"\n" | -10
  "\n"
  ;; dependencies
  "RDEPEND=\"\"\n"
  "DEPEND=\"${RDEPEND}\"\n"
  "BDEPEND=\"\"\n")


;;; Minor mode for editing files in an ebuild repository.

;; suppress byte-compiler warning in XEmacs
(defvar ebuild-repo-mode-hook)
(defvar ebuild-repo-mode-on-hook)
(defvar ebuild-repo-mode-off-hook)

(defun ebuild-repo-mode-before-save ()
  "Function to be called before saving a buffer.
This will be added to the `write-contents-functions' hook."
  (when ebuild-mode-fix-whitespace
    ;; trim trailing whitespace, except for patches
    (ebuild-mode-delete-trailing-whitespace)
    ;; tabify whitespace and squash multiple empty lines for ebuilds
    (when (and (derived-mode-p 'ebuild-mode)
	       (or (eq major-mode 'ebuild-mode)
		   (not (eq ebuild-mode-fix-whitespace 'ebuild))))
      (ebuild-mode-tabify)
      (ebuild-mode-squash-empty-lines)))
  (when ebuild-mode-update-copyright
    (ebuild-mode-update-copyright)
    ;; call it only once per buffer
    (set (make-local-variable 'ebuild-mode-update-copyright) nil))
  (when ebuild-mode-delete-cvs-line
    (ebuild-mode-delete-cvs-line))
  ;; return nil, otherwise the file is presumed to be written
  nil)

(defvar ebuild-repo-mode-map
  (make-sparse-keymap)
  "Keymap for `ebuild-repo-mode'.")

(defvar ebuild-mode-tag-line-regexp
  ".*[ \t]+<.*>[ \t]+([0-9]\\{4\\}-[01][0-9]-[0-3][0-9])$"
  "Regexp matching an author/date tag line in a configuration file.
This excludes `comment-start'.  See `ebuild-mode-insert-tag-line'
for the format of the tag line.")

;;;###autoload
(define-minor-mode ebuild-repo-mode
  "Minor mode for files in an ebuild repository."
  :lighter " Repo"
  :keymap ebuild-repo-mode-map
  (if (ignore-errors (check-coding-system 'utf-8-unix))
      ;; utf-8-unix doesn't exist in XEmacs 21.4
      (setq buffer-file-coding-system 'utf-8-unix))
  (static-if (not (featurep 'xemacs))
      (add-hook 'write-contents-functions
		#'ebuild-repo-mode-before-save t t)
    ;; make-local-hook gives a byte-compiler warning in GNU Emacs
    (make-local-hook 'write-contents-hooks)
    (add-hook 'write-contents-hooks
	      #'ebuild-repo-mode-before-save t t))
  (unless (local-variable-p 'fill-column (current-buffer)) ; XEmacs wants 2 args
    (setq fill-column 72))
  (unless (local-variable-p 'tab-width (current-buffer))
    (setq tab-width 4))
  (cond
   ((derived-mode-p 'conf-unix-mode)
    (unless (local-variable-p 'paragraph-separate (current-buffer))
      ;; Prevent fill-paragraph from rewrapping the paragraph into
      ;; a preceding author/date tag line
      (set (make-local-variable 'paragraph-separate)
	   (concat (default-value 'paragraph-separate)
		   "\\|^"
		   (regexp-quote (concat comment-start))
		   ebuild-mode-tag-line-regexp))))
   ((derived-mode-p 'nxml-mode)
    (eval-when-compile (ignore-errors (require 'nxml-mode)))
    (unless (or (local-variable-p 'nxml-child-indent (current-buffer))
		(local-variable-p 'nxml-attribute-indent (current-buffer)))
      (let ((indent (if ebuild-mode-xml-indent-tabs 4 2)))
	(set (make-local-variable 'nxml-child-indent) indent)
	(set (make-local-variable 'nxml-attribute-indent) (* 2 indent))))
    (unless (local-variable-p 'indent-tabs-mode (current-buffer))
      (setq indent-tabs-mode ebuild-mode-xml-indent-tabs)))))

;;;###autoload
(defun ebuild-repo-mode-maybe-enable ()
  "Enable `ebuild-repo-mode' when the file is in an ebuild repository."
  ;; We assume that we are in an ebuild repository if we find a file
  ;; "profiles/repo_name" in any (nth level) parent dir
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "profiles/repo_name")
       ;; Don't enable the mode for patches
       (not (string-match "\\.\\(diff\\|patch\\)\\'" buffer-file-name))
       (not (derived-mode-p 'diff-mode))
       (ebuild-repo-mode 1)))

(defun ebuild-mode-insert-tag-line ()
  "Insert a tag line with the user's name, e-mail address and date.
Format is \"# Larry The Cow <larry@gentoo.org> (2019-07-01)\".
This is intended for package.mask and similar configuration files
in a Gentoo profile."
  (interactive)
  (beginning-of-line)
  (insert (concat comment-start
		  (and (/= 0 (length comment-start))
		       (not (string-match "[ \t]" comment-start -1))
		       " "))
	  (format "%s <%s> (%s)\n"
		  ebuild-mode-full-name ebuild-mode-mail-address
		  (ebuild-mode-time-string "%Y-%m-%d"))))

;;; Key bindings.

(defvar ebuild-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    ;; C-<letter> keys for general commands
    (define-key map "\C-e" #'ebuild-run-command)
    (define-key map "\C-p" #'ebuild-mode-run-pkgdev)
    (define-key map "\C-c" #'ebuild-mode-run-pkgcheck)
    (define-key map "\C-w" #'ebuild-mode-find-workdir)
    (define-key map "\C-s" #'ebuild-mode-find-s)
    (define-key map "\C-d" #'ebuild-mode-find-image-dir)
    (define-key map "\C-l" #'ebuild-mode-find-build-log)
    (define-key map "\C-k" #'ebuild-mode-keyword)
    (define-key map "\C-y" #'ebuild-mode-ekeyword)
    (define-key map "\C-u" #'ebuild-mode-all-keywords-unstable)
    (define-key map "\C-n" #'ebuild-mode-insert-skeleton)
    ;; <letter> for ebuild subcommands
    (define-key map "l" 'ebuild-run-command-clean)
    (define-key map "c" 'ebuild-run-command-compile)
    (define-key map "g" 'ebuild-run-command-configure)
    (define-key map "f" 'ebuild-run-command-fetch)
    (define-key map "i" 'ebuild-run-command-install)
    (define-key map "d" 'ebuild-run-command-manifest) ; previously "digest"
    (define-key map "m" 'ebuild-run-command-merge)
    (define-key map "p" 'ebuild-run-command-prepare)
    (define-key map "q" 'ebuild-run-command-qmerge)
    (define-key map "t" 'ebuild-run-command-test)
    (define-key map "n" 'ebuild-run-command-unmerge)
    (define-key map "u" 'ebuild-run-command-unpack)
    map)
  "Keymap for `ebuild-mode' specific commands.")

(define-key ebuild-mode-map "\C-c\C-e" ebuild-mode-prefix-map)
(define-key ebuild-repo-mode-map "\C-c-" #'ebuild-mode-insert-tag-line)

;; Menu support for both Emacs and XEmacs.
(easy-menu-define ebuild-mode-menu ebuild-mode-map
  "Menu for `ebuild-mode'."
  `("Ebuild"
    ["Run ebuild command" ebuild-run-command
     :active (eq major-mode 'ebuild-mode)]
    ("ebuild commands"
     :active (eq major-mode 'ebuild-mode)
     ,@(mapcar (lambda (c)
		 (vector c (intern (concat "ebuild-run-command-" c))))
	       ebuild-commands-list))
    ["Run pkgdev command" ebuild-mode-run-pkgdev]
    ["Run pkgcheck command" ebuild-mode-run-pkgcheck]
    ["Find working directory (WORKDIR)" ebuild-mode-find-workdir
     :active (eq major-mode 'ebuild-mode)]
    ["Find build directory (S)" ebuild-mode-find-s
     :active (eq major-mode 'ebuild-mode)]
    ["Find image directory (D)" ebuild-mode-find-image-dir
     :active (eq major-mode 'ebuild-mode)]
    ["Find build log" ebuild-mode-find-build-log
     :active (eq major-mode 'ebuild-mode)]
    ["Insert ebuild skeleton" ebuild-mode-insert-skeleton
     :active (eq major-mode 'ebuild-mode)]
    ["Set/unset keyword" ebuild-mode-keyword]
    ["Set/unset keywords (ekeyword syntax)" ebuild-mode-ekeyword]
    ["Mark all keywords as unstable" ebuild-mode-all-keywords-unstable]
    ["Customize ebuild-mode" (customize-group 'ebuild)]))

(easy-menu-define ebuild-repo-mode-menu ebuild-repo-mode-map
  "Menu for `ebuild-repo-mode'."
  '("Ebuild"
    ;; show the menu only for conf files
    :visible (derived-mode-p 'conf-unix-mode)
    ["Insert package.mask tag line" ebuild-mode-insert-tag-line]
    ["Customize ebuild-mode" (customize-group 'ebuild)]))

(static-if (fboundp 'sh-must-be-shell-mode)
    ;; make TAB key work
    (defadvice sh-must-be-shell-mode
	(around ebuild-mode-sh-must-be-shell-mode activate)
      (unless (derived-mode-p 'sh-mode) ad-do-it)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . ebuild-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . ebuild-eclass-mode))

;;;###autoload
(add-hook
 ;; XEmacs 21.5 doesn't have find-file-hook
 (if (boundp 'find-file-hook) 'find-file-hook 'find-file-hooks)
 #'ebuild-repo-mode-maybe-enable)

(provide 'ebuild-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ebuild-mode.el ends here
