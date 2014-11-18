;;; ebuild-mode.el --- edit Gentoo ebuild and eclass files

;; Copyright 2006-2014 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Version: 1.27
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
  (or (fboundp 'delete-trailing-whitespace) ; exists in GNU Emacs only
      ;; from simple.el of Emacs 22.1
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

;; Predicate function for comparison of architecture keywords
;; (needed for variable definitions below)
(defun ebuild-mode-arch-lessp (a b)
  (let* ((tail (make-list 2 ""))
	 (as (nconc (split-string (or (car-safe a) a) "-") tail))
	 (bs (nconc (split-string (or (car-safe b) b) "-") tail)))
    ;; two-part keywords: first compare the OS (after hyphen, if any),
    ;; then the architecture (before hyphen)
    (if (string-equal (cadr as) (cadr bs))
	(string-lessp (car as) (car bs))
      (string-lessp (cadr as) (cadr bs)))))

(defvar ebuild-mode-portdir
  "/usr/portage"
  "Location of the Portage tree.")

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
   ;; could not read architectures from Portage tree, so fall back to default
   '("alpha" "amd64" "arm" "hppa" "ia64" "m68k" "mips" "ppc" "ppc64"
     "s390" "sh" "sparc" "x86" "amd64-fbsd" "sparc-fbsd" "x86-fbsd"))
  "List of architectures.")

(defvar ebuild-mode-arch-stable-list
  (or
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
   ebuild-mode-arch-list))

(defvar ebuild-mode-arch-regexp
  "^[ \t]*KEYWORDS=[\"']\\([^\"]*\\)[\"'][ \t]*$")

(defvar ebuild-mode-licenses
  (condition-case nil
      (directory-files (concat ebuild-mode-portdir "/licenses")
		       nil "\\`[^.]")
    (file-error nil))
  "List of licenses, determined from the Portage tree.")

(defvar ebuild-mode-eclasses
  (condition-case nil
      (mapcar
       (lambda (x) (substring x 0 (string-match "\\.eclass\\'" x)))
       (directory-files (concat ebuild-mode-portdir "/eclass")
			nil "\\.eclass\\'"))
     (file-error nil))
  "List of eclasses, determined from the Portage tree.")

(defvar ebuild-mode-restrict-list
  '("binchecks" "bindist" "fetch" "installsources" "mirror"
    "primaryuri" "strip" "test" "userpriv")
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
    "pretend" "qmerge" "rpm" "setup" "test" "unmerge" "unpack"))

;; suppress byte-compiler warning
(defvar ebuild-mode-menu)


;;; Font-lock.

(eval-and-compile
  (defun ebuild-mode-make-keywords-list (keywords-list face
						       &optional prefix suffix)
    ;; based on `generic-make-keywords-list' from generic.el
    ;; Note: XEmacs doesn't have generic.el
    (unless (listp keywords-list)
      (error "Keywords argument must be a list of strings"))
    (cons (concat prefix "\\<"
		  (let ((max-specpdl-size (max max-specpdl-size 2000)))
		    (regexp-opt keywords-list t))
		  "\\>" suffix)
	  face))

  (defun ebuild-mode-collect-equal-cdrs (src &optional limit)
    "For alist SRC, collect elements with equal cdr and concat their cars.
Optional argument LIMIT restarts collection after that number of elements."
    (let (dst e)
      (dolist (c src dst)
	(if (and (setq e (rassoc (cdr c) dst))
		 (not (and limit (> (length (car e)) limit))))
	    (setcar e (append (car e) (car c)))
	  (setq dst (cons (copy-sequence c) dst))))))
)

(eval-when-compile
  (load "ebuild-mode-keywords" nil t))

(defvar ebuild-mode-font-lock-keywords
  (eval-when-compile
    (mapcar
     (lambda (x) (apply 'ebuild-mode-make-keywords-list x))
     (ebuild-mode-collect-equal-cdrs
      (mapcar (lambda (x) (symbol-value (intern x)))
	      (all-completions "ebuild-mode-keywords-" obarray 'boundp))
      1000))))

;;; Mode definitions.

(defun ebuild-mode-tabify ()
  ;; Tabify whitespace at beginning of lines.
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

(defun ebuild-mode-before-save ()
  (delete-trailing-whitespace)
  (ebuild-mode-tabify)
  ;;(copyright-update)			; doesn't exist in XEmacs
  ;; return nil, otherwise the file is presumed to be written
  nil)

;;;###autoload
(define-derived-mode ebuild-mode shell-script-mode "Ebuild"
  "Major mode for Portage .ebuild and .eclass files."
  (if (featurep 'xemacs)
      ;; make-local-hook gives a byte-compiler warning in GNU Emacs
      (make-local-hook 'write-contents-hooks))
  (add-hook 'write-contents-hooks 'ebuild-mode-before-save t t)
  (sh-set-shell "bash")
  (easy-menu-add ebuild-mode-menu)	; needed for XEmacs
  (setq fill-column 72)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(add-hook 'ebuild-mode-hook
	  (lambda () (font-lock-add-keywords
		      nil ebuild-mode-font-lock-keywords)))

;;; Run ebuild command.

;;;###autoload
(defun ebuild-run-command (command)
  "Run ebuild COMMAND, with output to a compilation buffer."
  (interactive
   (list (completing-read "Run ebuild command: "
			  (mapcar 'list ebuild-commands-list)
			  nil t)))
  (or (member command ebuild-commands-list)
      (error "Ebuild command \"%s\" not known" command))
  (let ((file (file-relative-name buffer-file-name))
	(process-environment (cons "NOCOLOR=true" process-environment))
	;;(compilation-mode-hook (lambda () (setq truncate-lines t)))
	(compilation-buffer-name-function
	 (list 'lambda '(mode) (concat "*ebuild " command "*"))))
    (compile (format "ebuild %s %s" file command))))


;;; Modify package keywords.
;; This is basically a reimplementation of "ekeyword" in Emacs Lisp.

(defun ebuild-mode-get-keywords (&optional noerror)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (and (re-search-forward ebuild-mode-arch-regexp nil noerror)
	   (not (and (re-search-forward ebuild-mode-arch-regexp nil t)
		     (or noerror
			 (error "More than one KEYWORDS assignment found"))))
	   (mapcar (lambda (s)
		     (string-match "^\\([-~]?\\)\\(.*\\)" s)
		     (cons (match-string 2 s) (match-string 1 s)))
		   (split-string
		    ;;(match-string-no-properties 1) ; not in XEmacs 21.4
		    (buffer-substring-no-properties (match-beginning 1)
						    (match-end 1))))))))

(defun ebuild-mode-put-keywords (kw &optional noerror)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (and (re-search-forward ebuild-mode-arch-regexp nil noerror)
	   (not (and (re-search-forward ebuild-mode-arch-regexp nil t)
		     (or noerror
			 (error "More than one KEYWORDS assignment found"))))
	   (replace-match
	    (mapconcat (lambda (e) (concat (cdr e) (car e))) kw " ")
	    t t nil 1)))))

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
	 (t (setq keywords (append keywords (list k)))))))
    (ebuild-mode-put-keywords
     (sort keywords 'ebuild-mode-arch-lessp))))

(defun ebuild-mode-keyword (action arch)
  "Keyword manipulation."
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
		(cond ((null mode) 'try-completion)
		      ((eq mode t) 'all-completions)
		      ((eq mode 'lambda)
		       (if (fboundp 'test-completion)
			   'test-completion
			 ;; Emacs 21 and XEmacs don't have test-completion
			 (lambda (&rest args)
			   (eq (apply 'try-completion args) t))))
		      (t 'ignore))
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
  "Keyword manipulation. Accepts the same input format as ekeyword."
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
   "# Copyright 1999-" (format-time-string "%Y") " Gentoo Foundation\n"
   "# Distributed under the terms of the GNU General Public License v2\n"
   "# $Header: $\n"
   "\n"
   ;; EAPI
   "EAPI="
   (skeleton-read "EAPI: ")
   & "\n\n" | -5
   ;; inherited eclasses
   "inherit "
   ((completing-read
     "Eclass (null string to terminate): "
     (mapcar 'list ebuild-mode-eclasses))
    str & " ")
   & -1 & "\n\n" | -8
   ;; first variables block
   "DESCRIPTION=\"" (skeleton-read "Description: ") "\"\n"
   "HOMEPAGE=\"" (completing-read "Homepage: " '(("http://"))) "\"\n"
   "SRC_URI=\""
   (completing-read
    "Source URI: " (mapcar 'list '("http://" "ftp://" "mirror://")))
   "\"\n"
   "\n"
   ;; second variables block
   "LICENSE=\""
   ((completing-read
     "License (null string to terminate): "
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
   ((completing-read
     "USE flag (null string to terminate): "
     (mapcar 'list ebuild-mode-use-flags))
    str & " ")
   & -1 & "\"\n" | -6
   "RESTRICT=\""
   ((completing-read
     "RESTRICT (null string to terminate): "
     (mapcar 'list ebuild-mode-restrict-list))
    str & " ")
   & -1 & "\"\n" | -10
   "\n"
   ;; dependencies
   "DEPEND=\"\"\n"
   "RDEPEND=\"\$\{DEPEND\}\"\n")

;;; echangelog support.

(defun ebuild-run-echangelog (text)
  (interactive "sLog entry: ")
  (let ((bufname "*echangelog*"))
    (with-output-to-temp-buffer bufname
      (call-process "echangelog" nil (get-buffer bufname) nil text))))


;;; Keybindings.

;; sh-mode already uses the following C-c C-<letter> keys: cfilorstuwx
(define-key ebuild-mode-map "\C-c\C-e" 'ebuild-run-command)
(define-key ebuild-mode-map "\C-c\C-a" 'ebuild-run-echangelog)
(define-key ebuild-mode-map "\C-c\C-k" 'ebuild-mode-keyword)
(define-key ebuild-mode-map "\C-c\C-y" 'ebuild-mode-ekeyword)
(define-key ebuild-mode-map "\C-c\C-b" 'ebuild-mode-all-keywords-unstable)
(define-key ebuild-mode-map "\C-c\C-n" 'ebuild-mode-insert-skeleton)

;; Menu support for both Emacs and XEmacs.
(easy-menu-define ebuild-mode-menu ebuild-mode-map
  "Menu for ebuild-mode."
  `("Ebuild"
    ("Run ebuild command"
     ,@(mapcar (lambda (c) (vector c (list 'ebuild-run-command c)))
	       (sort (copy-sequence ebuild-commands-list) 'string-lessp)))
    ["Insert ebuild skeleton" ebuild-mode-insert-skeleton]
    ["Run echangelog" ebuild-run-echangelog]
    ["Set/unset keyword" ebuild-mode-keyword]
    ["Set/unset keywords (ekeyword syntax)" ebuild-mode-ekeyword]
    ["Mark all keywords as unstable" ebuild-mode-all-keywords-unstable]))

(and (< emacs-major-version 22)
     ;; make TAB key work
     (defadvice sh-must-be-shell-mode
       (around ebuild-mode-sh-must-be-shell-mode activate)
       (or (eq major-mode 'ebuild-mode)
	   ad-do-it)))

;;;###autoload
(add-to-list 'auto-mode-alist
	     '("\\.\\(ebuild\\|eclass\\|eblit\\)\\'" . ebuild-mode))

(provide 'ebuild-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ebuild-mode.el ends here
