;;; devbook-mode.el --- edit the Gentoo Devmanual  -*-lexical-binding:t-*-

;; Copyright 2020-2024 Gentoo Authors

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'nxml-mode)
(require 'rng-loc)
(require 'easymenu)
(require 'skeleton)

(defvar devbook-schema-file-name "devbook.rnc")

(defun devbook-locate-schema-file (&optional noerror)
  "Look for a devbook schema file in any parent directory.
If successful, load it as the schema for the current buffer.
Otherwise, signal an error, or return nil if the optional argument
NOERROR is non-nil."
  (interactive "P")
  (let* ((dir (and buffer-file-name
		   (locate-dominating-file buffer-file-name
					   devbook-schema-file-name)))
	 (file (and dir (expand-file-name devbook-schema-file-name dir))))
    (cond (file
	   (condition-case err
	       (progn
		 (rng-set-schema-file-1 file)
		 (rng-what-schema))
	     (error (unless noerror (signal (car err) (cdr err))))))
	  (noerror nil)
	  (t (error "Schema file %s not found" devbook-schema-file-name)))))

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
  (unless rng-current-schema-file-name
    (devbook-locate-schema-file t)))

(define-skeleton devbook-insert-skeleton
  "Insert a skeleton for a DevBook XML document."
  nil
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<guide "
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
  "</guide>\n")

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
