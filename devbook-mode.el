;;; devbook-mode.el --- edit the Gentoo Devmanual  -*-lexical-binding:t-*-

;; Copyright 2020-2026 Gentoo Authors

;; Author: Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: text, languages

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

;; Minimum Emacs version:
;; GNU Emacs 26.3
;; XEmacs not supported

;;; Code:

(require 'nxml-mode)
(require 'rng-loc)
(require 'easymenu)
(require 'skeleton)

(defvar devbook-schema-file-name "devbook.rnc")

(defun devbook-set-schema (&optional noerror)
  "Set the schema for this buffer.
Call `rng-locate-schema-file' first, which tries to locate a schema
based on the buffer's contents and file-name.  If unsuccessful,
look for a devbook schema file in any parent directory.  If no
schema file could be found either way, use the vacuous schema which
allows any well-formed XML.

Optional argument NOERROR suppresses signalling of any errors.
Return the schema file name, or nil if no schema was found."
  (interactive "P")
  (condition-case err
      (rng-set-schema-file-1
       (or (cl-letf* ((origfn (symbol-function 'rng-document-element))
		      ;; make sure that rng-document-element returns
		      ;; a document element even if the buffer is empty
		      ((symbol-function 'rng-document-element)
		       (lambda ()
			 (or (funcall origfn)
			     '(nil nil "devbook")))))
	     (rng-locate-schema-file))
	   (let ((dir (and buffer-file-name
			   (locate-dominating-file buffer-file-name
						   devbook-schema-file-name))))
	     (and dir (expand-file-name devbook-schema-file-name dir)))))
    (error (unless noerror (signal (car err) (cdr err)))))
  (unless noninteractive (rng-what-schema))
  rng-current-schema-file-name)

(defun devbook-fill-tag-nobreak-p ()
  "Return non-nil if point is inside a tag right after its name.
This is used in `fill-nobreak-predicate' to prevent breaking a line
between the element name and its first attribute."
  (save-excursion
    (skip-chars-backward " \t")
    (when (/= (skip-chars-backward "[:alnum:]_.:-") 0)
      (skip-chars-backward "/?!")
      (eq (char-before) ?<))))

;;;###autoload
(define-derived-mode devbook-mode nxml-mode "DevBook"
  "Major mode for editing the Gentoo Devmanual."
  ;; The style guide says 80 characters. Set to 79 to keep diff output
  ;; within the limit (and arguably, 80 includes the newline).
  (setq fill-column 79)
  (setq indent-tabs-mode nil)
  ;; Tabs are allowed in ebuild codesamples, so this isn't redundant.
  (setq tab-width 4)
  ;; *** FIXME *** The style guide says no indentation, except inside
  ;; <tr>, <ul>, <ol> and <dl>, where it must be 2 spaces. There is no
  ;; easy way to achieve this, so set to 0 which is right more often.
  (set (make-local-variable 'nxml-child-indent) 0)
  (add-hook 'fill-nobreak-predicate #'devbook-fill-tag-nobreak-p nil t)
  (unless rng-current-schema-file-name
    (devbook-set-schema t)))

(define-skeleton devbook-insert-skeleton
  "Insert a skeleton for a DevBook XML document."
  nil
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<devbook "
  (let ((path (if (and buffer-file-name
		       (string-match "/devmanual[^/]*/\\(.*/\\)"
				     buffer-file-name))
		  (match-string 1 buffer-file-name)
		(skeleton-read "Path: "))))
    (if (string-match "\\`/*\\'" path)
	"root=\"true\""
      (concat "self=\"" (file-name-as-directory path) "\"")))
  ">\n"
  "<chapter>\n"
  "<title>" (skeleton-read "Title: ") "</title>\n"
  - "\n"
  "</chapter>\n"
  "</devbook>\n")

(define-key devbook-mode-map "\C-c\C-n" #'devbook-insert-skeleton)

(easy-menu-define devbook-mode-menu devbook-mode-map
  "Menu for `devbook-mode'."
  '("DevBook"
    ["Insert skeleton" devbook-insert-skeleton]))

;;;###autoload
(add-to-list 'auto-mode-alist '("/devmanual.*\\.xml\\'" . devbook-mode))

(provide 'devbook-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; devbook-mode.el ends here
