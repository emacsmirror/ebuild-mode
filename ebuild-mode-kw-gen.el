;;; ebuild-mode-kw-gen.el --- Generate eclass keywords  -*-lexical-binding:t-*-

;; Copyright 2026 Gentoo Authors

;; Author: Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: maint

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate a list of eclass keywords for ebuild-mode-keywords.el.

;;; Code:

(require 'ebuild-mode)
(require 'ebuild-mode-keywords)

(defvar emkg-eclass-dir (expand-file-name "eclass" ebuild-mode-portdir))

(defvar emkg-target-file "ebuild-mode-keywords.el")

(defvar emkg-regex-begin "@@KEYWORDS-BEGIN@@.*\n")
(defvar emkg-regex-end "^.*@@KEYWORDS-END@@")

(defvar emkg-obsolete-eclasses nil)

(defun emkg-get-eclass-functions (name)
  "Return a list of documented non-internal functions in eclass NAME."
  (let ((file (expand-file-name (concat name ".eclass") emkg-eclass-dir))
	(coding-system-for-read 'utf-8-unix)
	functions)
    (with-temp-buffer
      (insert-file-contents file)
      (if (re-search-forward "^#[ \t]*@DEAD\\>" nil t)
	  'dead
	(while (re-search-forward
		"^#[ \t]*@FUNCTION:[ \t]*\\(\\S-+\\)" nil t)
	  (let ((fn (match-string-no-properties 1)))
	    (while (progn
		     (forward-line 1)
		     (and (looking-at "^#")
			  (or (not (looking-at "^#[ \t]*@INTERNAL\\>"))
			      (setq fn nil)))))
	    (and fn
		 ;; internal function (name starts with an underscore)?
		 (not (eq ?_ (aref fn 0)))
		 ;; package manager function?
		 (not (member fn (car ebuild-mode-keywords-0)))
		 ;; sanity check: is there an actual function definition?
		 (or (save-excursion
		       (let ((qfn (regexp-quote fn)))
			 (re-search-forward
			  ;; FOO() or function FOO
			  (concat "^[ \t]*\\(?:" qfn "[ \t]*([ \t]*)"
				  "\\|function[ \t]+" qfn "[ \t\n(]\\)")
			  nil t)))
		     (prog1 nil
		       (lwarn 'ebuild nil
			      "%s: %s documented but not found" name fn)))
		 (push fn functions))))
	(nreverse functions)))))

(defun emkg-get-all-eclass-keywords (prefix column)
  "Get eclass keywords, formatted for `ebuild-mode-keywords-eclass'.
PREFIX and COLUMN as used to set `fill-prefix' and `fill-column'."
  (with-temp-buffer
    (let ((eclasses
	   (sort
	    (mapcar #'file-name-sans-extension
		    (directory-files emkg-eclass-dir nil "\\.eclass\\'" t))
	    #'string-lessp))
	  (fill-prefix prefix)
	  (fill-column column))
      (dolist (name eclasses)
	(let ((functions (emkg-get-eclass-functions name)))
	  (cond
	   ((eq functions 'dead)
	    (message "%-24s: skip (dead)" name))
	   ((member name emkg-obsolete-eclasses)
	    (message "%-24s: skip (obsolete)" name))
	   ((null functions)
	    ;;(message "%-24s:  no functions" name)
	    )
	   (t
	    ;;(message "%-24s: %3d functions" name (length functions))
	    (insert fill-prefix (format ";; %s\n" name))
	    (let ((start (point)))
	      (insert fill-prefix
		      (mapconcat #'prin1-to-string functions " ")
		      "\n")
	      (fill-region start (point))))))))
    (buffer-string)))

(defun emkg-update-eclass-keywords ()
  "Visit `emkg-target-file' and update its list of eclass keywords."
  (interactive)
  (find-file emkg-target-file)
  (goto-char (point-min))
  (let ((start (re-search-forward emkg-regex-begin))
	(end (and (re-search-forward emkg-regex-end)
		  (goto-char (match-beginning 0))))
	(keywords (emkg-get-all-eclass-keywords
		   (make-string (current-indentation) ?\ ) fill-column)))
    (if (string-equal (buffer-substring start end) keywords)
	(message "No need to update %s" emkg-target-file)
      (delete-region start end)
      (insert keywords)
      (save-buffer)
      (message "Success: %s updated" emkg-target-file))))

(provide 'ebuild-mode-kw-gen)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; ebuild-mode-kw-gen.el ends here
