;;; ebuild-mode.el --- edit Gentoo ebuild and eclass files

;; Copyright 2006-2023 Gentoo Authors

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Version: 1.65
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


;;; Compatibility code.

(eval-and-compile
  (or (fboundp 'delete-trailing-whitespace) ; doesn't exist in XEmacs 21.4
      ;; from simple.el of GNU Emacs 22.1
(defun delete-trailing-whitespace ()
  "Delete all the trailing whitespace across the current buffer.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by \\[narrow-to-region] and friends.
A formfeed is not considered whitespace by this function."
  (interactive "*")
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\s-$" nil t)
	(skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
	;; Don't delete formfeeds, even if they are considered whitespace.
	(save-match-data
	  (if (looking-at ".*\f")
	      (goto-char (match-end 0))))
	(delete-region (point) (match-end 0))))))
))


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

(defcustom ebuild-mode-fix-whitespace t
  "If non-nil, fix whitespace before writing a file.
Namely, delete trailing whitespace and tabify whitespace at beginning
of lines."
  :type 'boolean
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
    '("NO_COLOR=1" "NOCOLOR=true"))
  "List of additional environment variables for subprocesses.
Each element should be a string of the form NAME=VALUE. This will
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
	 (sort (split-string (buffer-string)) 'ebuild-mode-arch-lessp))
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
	   (sort archs 'ebuild-mode-arch-lessp)))
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
	   (sort archs 'ebuild-mode-arch-lessp)))
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
    "fetch" "fetchall" "help" "info" "install" "manifest" "merge"
    "package" "postinst" "postrm" "preinst" "prepare" "prerm"
    "pretend" "qmerge" "rpm" "setup" "test" "unmerge" "unpack")
  "List of ebuild sub-commands.")

(defvar ebuild-mode-pkgdev-commands
  '("commit" "manifest" "mask" "push" "showkw")
  "List of pkgdev sub-commands.")

(defvar ebuild-mode-pkgcheck-commands
  '("cache" "ci" "replay" "scan" "show")
  "List of pkgcheck sub-commands.")

;; suppress byte-compiler warning in XEmacs
(defvar ebuild-mode-menu)


;;; Font-lock.

(eval-when-compile
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
	  (setq dst (cons (copy-sequence c) dst)))
	 (t
	  ;; otherwise, split the new element into chunks of length LIMIT
	  (let ((cc (car c)))
	    (while cc
	      (setq dst (cons (cons (last cc limit) (cdr c)) dst))
	      (setq cc (butlast cc limit)))))))))

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

(defun ebuild-mode-time-string (format-string &optional time)
  "Use FORMAT-STRING to format the time value TIME.
Calls `format-time-string' (which see) for the UTC time zone.
Compatibility function for XEmacs."
  (if (featurep 'xemacs)
      ;; format-time-string in XEmacs 21.5 can take only two arguments,
      ;; i.e. it doesn't support ZONE
      (let ((process-environment (copy-sequence process-environment))
	    (tz (getenv "TZ")))
	(setenv "TZ" "UTC")
	(unwind-protect
	    (format-time-string format-string time)
	  ;; This is needed because setenv handles TZ specially.
	  ;; So, restoring the environment is not enough.
	  (setenv "TZ" tz)))
    (format-time-string format-string time t)))

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
		  (if (or (> 1999 y1) (>= y1 y2) (> y2 y))
		      ;; XEmacs wants 'warning instead of :warning,
		      ;; but nil always works (and defaults to :warning)
		      (lwarn 'ebuild nil
			     "Suspicious range of copyright years: %d-%d" y1 y2)
		    (if (/= y2 y)
			(replace-match year t t nil 2)))
		;; Update single year and convert to range if necessary
		(if (or (> 1999 y1) (> y1 y))
		    (lwarn 'ebuild nil "Suspicious copyright year: %d" y1)
		  (if (/= y1 y)
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
(define-derived-mode ebuild-mode shell-script-mode "Ebuild"
  "Major mode for Gentoo .ebuild and .eclass files."
  ;; Always enable ebuild-repo-mode, even if the ebuild is edited
  ;; outside an ebuild repository
  (ebuild-repo-mode 1)
  (sh-set-shell "bash")
  (if (featurep 'xemacs)
      (easy-menu-add ebuild-mode-menu))
  (setq fill-column 72)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (and buffer-file-name
       (let ((case-fold-search t))
	 (string-match "\\.eclass\\'"
		       (file-name-sans-versions buffer-file-name)))
       ;; Eclass documentation uses two spaces after sentence ends
       (set (make-local-variable 'sentence-end-double-space) t)))

(add-hook 'ebuild-mode-hook
	  (lambda () (font-lock-add-keywords
		      nil ebuild-mode-font-lock-keywords)))

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
		     'ebuild-mode-ebuild-cmd-complete
		     nil nil nil 'ebuild-mode-ebuild-history)))
  (or buffer-file-name
      (error "No file for this buffer"))
  (let* ((file (file-relative-name buffer-file-name))
	 (shell-command (format "ebuild %s %s" file command))
	 (process-environment (append ebuild-mode-process-environment
				      process-environment))
	 ;;(compilation-mode-hook (lambda () (setq truncate-lines t)))
	 (compilation-buffer-name-function (lambda (mode) "*ebuild*")))
    (if (featurep 'xemacs)
	(compile shell-command)
      (compile shell-command ebuild-log-buffer-mode))))

(define-derived-mode ebuild-compilation-mode compilation-mode "Compilation"
  "Like `compilation-mode' but with color support.
Translates ANSI SGR control sequences into text properties (if the
Emacs version supports it). Variable `ansi-color-for-compilation-mode'
must be non-nil for this to have any effect."
  (if (fboundp 'ansi-color-compilation-filter)
      (add-hook 'compilation-filter-hook
		'ansi-color-compilation-filter nil t)))

(defun ebuild-mode-get-completion-function (mode)
  "Get completion function for completion mode MODE."
  (cond ((null mode) 'try-completion)
	((eq mode t) 'all-completions)
	((eq mode 'lambda)
	 (if (fboundp 'test-completion)
	     'test-completion
	   ;; XEmacs 21.4 doesn't have test-completion
	   (lambda (&rest args)
	     (eq (apply 'try-completion args) t))))
	(t 'ignore)))

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
			  'ebuild-mode-command-complete
			  nil nil "pkgdev "
			  'ebuild-mode-pkgdev-history)))
  (let ((process-environment (append ebuild-mode-process-environment
				     process-environment))
	(compilation-buffer-name-function (lambda (mode) "*pkgdev*")))
    (if (featurep 'xemacs)
	(compile command)
      (compile command ebuild-log-buffer-mode))))

(defun ebuild-mode-run-pkgcheck (command)
  "Run pkgcheck COMMAND with output to a compilation buffer.
Like `compile', but with autocompletion for pkgcheck."
  (interactive
   (list (completing-read "Run pkgcheck command: "
			  'ebuild-mode-command-complete
			  nil nil "pkgcheck "
			  'ebuild-mode-pkgcheck-history)))
  (let ((process-environment (append ebuild-mode-process-environment
				     process-environment))
	(compilation-buffer-name-function (lambda (mode) "*pkgcheck*")))
    (if (featurep 'xemacs)
	(compile command)
      (compile command ebuild-log-buffer-mode))))

(defun ebuild-mode-find-workdir (&optional other-window)
  "Visit Portage's working directory for the ebuild in this buffer.
With prefix argument OTHER-WINDOW, visit the directory in another window."
  (interactive "P")
  (unless buffer-file-name
    (error "No file for this buffer"))
  (let* ((pkgdir (directory-file-name (file-name-directory buffer-file-name)))
	 (catdir (directory-file-name (file-name-directory pkgdir)))
	 (category (file-name-nondirectory catdir))
	 (pn (file-name-nondirectory pkgdir))
	 (pf (file-name-sans-extension
	      (file-name-nondirectory buffer-file-name))))
    ;; sanity check
    (unless (and (file-exists-p
		  (expand-file-name "../profiles/repo_name" catdir))
		 (string-match (concat "\\`" (regexp-quote pn) "-") pf)
		 (string-match "\\.ebuild\\'"
			       (file-name-sans-versions buffer-file-name)))
      (error "This doesn't look like an ebuild repository"))
    (let ((workdir (concat (file-name-as-directory ebuild-mode-portage-tmpdir)
			   (file-name-as-directory category)
			   (file-name-as-directory pf)
			   "work")))
      (unless (file-directory-p workdir)
	(error "Portage workdir doesn't exist"))
      (if other-window
	  (find-file-other-window workdir)
	(find-file workdir)))))

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
  "Set keywords. KW is an alist of architectures and leaders."
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
     (sort keywords 'ebuild-mode-arch-lessp))))

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
		     (mapcar 'list
			     (append '("all" "*") ebuild-mode-arch-list))
		     nil t)))
  (ebuild-mode-modify-keywords (list (cons arch action))))

(defun ebuild-mode-ekeyword-complete (s predicate mode)
  "Completion function, to be used as second argument of `completing-read'.
Return common substring of all completions of S for given PREDICATE.
MODE can be nil, t, or `lambda'. See documentation of `try-completion'
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
		(mapcar 'list
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
  "Keyword manipulation. Accepts the same input format as ekeyword.
KEYWORDS is a whitespace separated string containing the keywords
that shall be manipulated."
  (interactive
   (list (completing-read "Keywords: " 'ebuild-mode-ekeyword-complete)))
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
   "EAPI: " (mapcar 'list ebuild-mode-eapi-list)
   nil nil (car (last ebuild-mode-eapi-list))) ; default to most recent EAPI
  "\n"
  "\n"
  ;; inherited eclasses
  "inherit "
  ((completing-read "Eclass (null string to terminate): "
		    (mapcar 'list ebuild-mode-eclasses))
   str & " ")
  & -1 & "\n\n" | -8
  ;; first variables block
  "DESCRIPTION=\"" (skeleton-read "Description: ") "\"\n"
  "HOMEPAGE=\""
  (completing-read "Homepage: "
		   (mapcar 'list ebuild-mode-protocols-homepage))
  "\"\n"
  "SRC_URI=\""
  (completing-read "Source URI: "
		   (mapcar 'list ebuild-mode-protocols-src_uri))
  "\"\n"
  "\n"
  ;; second variables block
  "LICENSE=\""
  ((completing-read "License (null string to terminate): "
		    (mapcar 'list ebuild-mode-licenses))
   str & " ")
  & -1 "\"\n"
  "SLOT=\"0\"\n"
  "KEYWORDS=\""
  ((completing-read
    "Keyword (null string to terminate): "
    (nconc
     (mapcar (lambda (x) (list (concat "~" x))) ebuild-mode-arch-list)
     (mapcar 'list ebuild-mode-arch-stable-list)))
   str & " ")
  & -1 "\"\n"
  "IUSE=\""
  ((completing-read "USE flag (null string to terminate): "
		    (mapcar 'list ebuild-mode-use-flags))
   str & " ")
  & -1 & "\"\n" | -6
  "RESTRICT=\""
  ((completing-read "RESTRICT (null string to terminate): "
		    (mapcar 'list ebuild-mode-restrict-list))
   str & " ")
  & -1 & "\"\n" | -10
  "\n"
  ;; dependencies
  "RDEPEND=\"\"\n"
  "DEPEND=\"${RDEPEND}\"\n")


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
    (delete-trailing-whitespace)
    ;; tabify whitespace and squash multiple empty lines for ebuilds
    (when (derived-mode-p 'ebuild-mode)
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

;;;###autoload
(define-minor-mode ebuild-repo-mode
  "Minor mode for files in an ebuild repository."
  :lighter " Repo"
  :keymap ebuild-repo-mode-map
  (if (ignore-errors (check-coding-system 'utf-8-unix))
      ;; utf-8-unix doesn't exist in XEmacs 21.4
      (setq buffer-file-coding-system 'utf-8-unix))
  (if (not (featurep 'xemacs))
      (add-hook 'write-contents-functions 'ebuild-repo-mode-before-save t t)
    ;; make-local-hook gives a byte-compiler warning in GNU Emacs
    (make-local-hook 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'ebuild-repo-mode-before-save t t))
  (unless (local-variable-p 'fill-column (current-buffer)) ; XEmacs wants 2 args
    (setq fill-column 72))
  (unless (local-variable-p 'tab-width (current-buffer))
    (setq tab-width 4))
  (when (derived-mode-p 'nxml-mode)
    (eval-when-compile (ignore-errors (require 'nxml-mode)))
    (unless (or (local-variable-p 'nxml-child-indent (current-buffer))
		(local-variable-p 'nxml-attribute-indent (current-buffer)))
      (let ((indent (if ebuild-mode-xml-indent-tabs 4 2)))
	(set (make-local-variable 'nxml-child-indent) indent)
	(set (make-local-variable 'nxml-attribute-indent) (* 2 indent))))
    (unless (local-variable-p 'indent-tabs-mode (current-buffer))
      (setq indent-tabs-mode ebuild-mode-xml-indent-tabs))))

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
  (insert (format "%s %s <%s> (%s)\n"
		  comment-start
		  ebuild-mode-full-name ebuild-mode-mail-address
		  (ebuild-mode-time-string "%Y-%m-%d"))))

;;; Keybindings.

;; sh-mode already uses the following C-c C-<letter> keys: cfilorstuwx
(define-key ebuild-mode-map "\C-c\C-e" 'ebuild-run-command)
(define-key ebuild-mode-map "\C-c\C-p" 'ebuild-mode-run-pkgdev)
(define-key ebuild-mode-map "\C-c\C-q" 'ebuild-mode-run-pkgcheck)
(define-key ebuild-mode-map "\C-c\C-d" 'ebuild-mode-find-workdir)
(define-key ebuild-mode-map "\C-c\C-k" 'ebuild-mode-keyword)
(define-key ebuild-mode-map "\C-c\C-y" 'ebuild-mode-ekeyword)
(define-key ebuild-mode-map "\C-c\C-b" 'ebuild-mode-all-keywords-unstable)
(define-key ebuild-mode-map "\C-c\C-n" 'ebuild-mode-insert-skeleton)
(define-key ebuild-repo-mode-map "\C-c-" 'ebuild-mode-insert-tag-line)

;; Menu support for both Emacs and XEmacs.
(easy-menu-define ebuild-mode-menu ebuild-mode-map
  "Menu for `ebuild-mode'."
  `("Ebuild"
    ("ebuild commands"
     ["Run ebuild command" ebuild-run-command]
     ,@(mapcar (lambda (c) (vector c (list 'ebuild-run-command c)))
	       (sort (copy-sequence ebuild-commands-list) 'string-lessp)))
    ["Run pkgdev command" ebuild-mode-run-pkgdev]
    ["Run pkgcheck command" ebuild-mode-run-pkgcheck]
    ["Find Portage workdir" ebuild-mode-find-workdir]
    ["Insert ebuild skeleton" ebuild-mode-insert-skeleton]
    ["Set/unset keyword" ebuild-mode-keyword]
    ["Set/unset keywords (ekeyword syntax)" ebuild-mode-ekeyword]
    ["Mark all keywords as unstable" ebuild-mode-all-keywords-unstable]
    ["Customize ebuild-mode" (customize-group 'ebuild)]))

(easy-menu-define ebuild-repo-mode-menu ebuild-repo-mode-map
  "Menu for `ebuild-repo-mode'."
  `("Ebuild"
    ;; show the menu only for conf files
    :visible (derived-mode-p 'conf-unix-mode)
    ["Insert package.mask tag line" ebuild-mode-insert-tag-line]
    ["Customize ebuild-mode" (customize-group 'ebuild)]))

(and (< emacs-major-version 22)
     ;; make TAB key work
     (defadvice sh-must-be-shell-mode
	 (around ebuild-mode-sh-must-be-shell-mode activate)
       (or (eq major-mode 'ebuild-mode)
	   ad-do-it)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(ebuild\\|eclass\\)\\'" . ebuild-mode))

;;;###autoload
(add-hook
 ;; XEmacs 21.5 doesn't have find-file-hook
 (if (boundp 'find-file-hook) 'find-file-hook 'find-file-hooks)
 'ebuild-repo-mode-maybe-enable)

(provide 'ebuild-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ebuild-mode.el ends here
