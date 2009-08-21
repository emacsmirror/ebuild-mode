;;; gentoo-syntax.el --- modes for editing Gentoo specific files

;; Copyright 2006-2009 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Version: 1.12
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

;; suppress byte-compiler warning
(defvar ebuild-mode-menu)

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

  (defun ebuild-mode-collect-equal-cdrs (src)
    "For alist SRC, collect elements with equal cdr and concat their cars."
    (let (dst e)
      (dolist (c src dst)
	(if (setq e (rassoc (cdr c) dst))
	    (setcar e (append (car e) (car c)))
	  (setq dst (cons (copy-sequence c) dst))))))
)

(eval-when-compile
  (load "ebuild-mode-keywords" nil t)
  (load "eselect-mode-keywords" nil t))

(defvar ebuild-mode-font-lock-keywords
  (eval-when-compile
    (mapcar
     (lambda (x)
       (apply 'ebuild-mode-make-keywords-list x))
     (ebuild-mode-collect-equal-cdrs
      (mapcar
       (lambda (x) (symbol-value (intern x)))
       (all-completions "ebuild-mode-keywords-" obarray 'boundp))))))

(defvar eselect-mode-font-lock-keywords
  (eval-when-compile
    (mapcar
     (lambda (x)
       (apply 'ebuild-mode-make-keywords-list x))
     (ebuild-mode-collect-equal-cdrs
      (mapcar
       (lambda (x) (symbol-value (intern x)))
       (all-completions "eselect-mode-keywords-" obarray 'boundp))))))

(font-lock-add-keywords 'ebuild-mode ebuild-mode-font-lock-keywords)
(font-lock-add-keywords 'eselect-mode eselect-mode-font-lock-keywords)

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
  (setq tab-width 4)
  (setq indent-tabs-mode t))

;;;###autoload
(define-derived-mode eselect-mode shell-script-mode "Eselect"
  "Major mode for .eselect files."
  (if (featurep 'xemacs)
      (make-local-hook 'write-contents-hooks))
  (add-hook 'write-contents-hooks 'ebuild-mode-before-save t t)
  (sh-set-shell "bash")
  (setq tab-width 4)
  (setq indent-tabs-mode t))


;;; Run ebuild command.

(defvar ebuild-commands-list
  '("help" "setup" "fetch" "digest" "manifest" "unpack" "compile"
    "test" "preinst" "postinst" "install" "qmerge" "merge"
    "prerm" "postrm" "unmerge" "config" "package" "rpm" "clean"))

;;;###autoload
(defun ebuild-run-command (command)
  "Run ebuild COMMAND, with output to a compilation buffer."
  (interactive
   (list (completing-read "Run ebuild command: "
			  (mapcar 'list ebuild-commands-list)
			  nil t)))
  (or (member command ebuild-commands-list)
      (error "Ebuild command \"%s\" not known" command))
  (let ((process-environment
	 (cons "NOCOLOR=true" process-environment))
	;;(compilation-mode-hook
	;; (lambda () (setq truncate-lines t)))
	(compilation-buffer-name-function
	 (list 'lambda '(mode) (concat "*ebuild " command "*"))))
    (compile (format "ebuild %s %s" buffer-file-name command))))


;;; Modify package keywords.
;; This is basically a reimplementation of "ekeyword" in Emacs Lisp.

(defvar ebuild-mode-arch-stable-list
  '("alpha" "amd64" "arm" "hppa" "ia64" "m68k" "ppc" "ppc64"
    "s390" "sh" "sparc" "x86"))

(defvar ebuild-mode-arch-list
  `(,@ebuild-mode-arch-stable-list "mips" "sparc-fbsd" "x86-fbsd"))

(defvar ebuild-mode-arch-regexp
  "^KEYWORDS=[\"']\\([^\"]*\\)[\"'][ \t]*$")

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

(defun ebuild-mode-sort-keywords (kw)
  (sort kw
	(lambda (a b)
	  (cond ((string-equal (car a) "*") t)
		((string-equal (car b) "*") nil)
		(t
		 ;; split two-part keywords: first compare the OS
		 ;; (after hyphen, if any), then the arch (before hyphen)
		 (let ((as (split-string (car a) "-"))
		       (bs (split-string (car b) "-")))
		   (if (equal (cadr as) (cadr bs))
		       (string-lessp (or (car as) "") (or (car bs) ""))
		     (string-lessp (or (cadr as) "") (or (cadr bs) "")))))))))

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
     (ebuild-mode-sort-keywords keywords))))

(defvar ebuild-mode-action-alist
  '(("unstable" . "~")
    ("stable" . "")
    ("mask" . "-")
    ("drop" . nil)))

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

;;; Skeleton support.

;; List of popular licenses.
;; From statistics at <http://gpnl.larrythecow.org/stats.php?q=license>
(defvar ebuild-mode-common-licenses
  '("GPL-2" "BSD" "LGPL-2.1" "Artistic" "as-is" "LGPL-2" "MIT" "GPL-3"
    "public-domain" "Apache-2.0" "freedist"))

(define-skeleton ebuild-mode-insert-skeleton
  "Insert a starting point for an ebuild."
   nil
   "# Copyright 1999-" (format-time-string "%Y") " Gentoo Foundation\n"
   "# Distributed under the terms of the GNU General Public License v2\n"
   "# $Header: $\n"
   "\n"
   (let ((s (skeleton-read "EAPI: ")))
     (if (or (string= "" s) (string= "0" s))
	 ""
       (concat "EAPI=" s "\n\n")))
   "DESCRIPTION=\"" (skeleton-read "Description: ") "\"\n"
   "HOMEPAGE=\"" (completing-read "Homepage: " '(("http://"))) "\"\n"
   "SRC_URI=\""
   (completing-read
    "Source URI: " (mapcar 'list '("http://" "ftp://" "mirror://")))
   "\"\n"
   "\n"
   "LICENSE=\""
   ((completing-read
     "License (null string to terminate): "
     (mapcar 'list ebuild-mode-common-licenses))
    str & " ")
   -1 "\"\n"
   "SLOT=\"0\"\n"
   "KEYWORDS=\""
   ((completing-read
     "Keyword (null string to terminate): "
     (mapcar (lambda (x) (list (concat "~" x))) ebuild-mode-arch-list))
    str & " ")
   -1 "\"\n"
   "IUSE=\"\"\n"
   "\n"
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
    ["Set/unset keywords (ekeyword syntax)" ebuild-mode-ekeyword]))

(and (< emacs-major-version 22)
     ;; make TAB key work
     (defadvice sh-must-be-shell-mode
       (around ebuild-mode-sh-must-be-shell-mode activate)
       (or (memq major-mode '(ebuild-mode eselect-mode))
	   ad-do-it)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . ebuild-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . ebuild-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eselect\\'" . eselect-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("runscript" . sh-mode))

(provide 'gentoo-syntax)
(provide 'ebuild-mode)			; backwards compatibility

;; Local Variables:
;; coding: utf-8
;; End:

;;; gentoo-syntax.el ends here
