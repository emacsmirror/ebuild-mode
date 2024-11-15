;;; gentoo-newsitem-mode-tests.el  -*-lexical-binding:t-*-

;; Copyright 2024 Gentoo Authors

;; Author: Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>

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

;;; Code:

(require 'ert)
(require 'gentoo-newsitem-mode)

(eval-when-compile
  (unless (fboundp 'cl-letf)
    (defalias 'cl-letf  #'letf)
    (defalias 'cl-letf* #'letf*)))

(ert-deftest gentoo-newsitem-test-font-lock ()
  (with-temp-buffer
    (gentoo-newsitem-mode)
    (insert "Author: Larry the Cow\n")
    (if (fboundp 'font-lock-ensure)
	(font-lock-ensure)
      (font-lock-fontify-region (point-min) (point-max)))
    (goto-char (point-min))
    (search-forward "Author")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-keyword-face))
    (search-forward "Larry")
    (should-not (get-text-property (match-beginning 0) 'face))))

(ert-deftest gentoo-newsitem-test-skeleton ()
  (with-temp-buffer
    (cl-letf* ((testinput
		'("Skeleton test"			 ; Title
		  "Larry the Cow <larry@example.org>" "" ; Author
		  ""					 ; Translator
		  "2024-08-10"				 ; Posted
		  ""					 ; News-Item-Format
		  ""					 ; Display-If-Installed
		  ""					 ; Display-If-Keyword
		  ""))					 ; Display-If-Profile
	       (getinput (lambda (&rest _args)
			   (concat (pop testinput))))
	       ((symbol-function 'read-from-minibuffer) getinput)
	       ((symbol-function 'read-string) getinput)
	       ;; prevent a segfault (seen with XEmacs 21.4.24 and 21.5.35)
	       ;; https://foss.heptapod.net/xemacs/xemacs/-/issues/6
	       ((symbol-function 'pos-visible-in-window-p)
		(lambda (&rest _args) t)))
      (gentoo-newsitem-insert-skeleton))
    (should (string-equal
	     (buffer-string)
	     (concat "Title: Skeleton test\n"
		     "Author: Larry the Cow <larry@example.org>\n"
		     "Posted: 2024-08-10\n"
		     "Revision: 1\n"
		     "News-Item-Format: 2.0\n\n")))))

(ert-deftest gentoo-newsitem-test-keybindings ()
  (should (equal (lookup-key gentoo-newsitem-mode-map "\C-c\C-n")
		 'gentoo-newsitem-insert-skeleton))
  (with-temp-buffer
    (gentoo-newsitem-mode)
    (should (equal (local-key-binding "\C-c\C-n")
		   'gentoo-newsitem-insert-skeleton))))

(provide 'gentoo-newsitem-mode-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; gentoo-newsitem-mode-tests.el ends here
