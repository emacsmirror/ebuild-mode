;;; glep-mode-tests.el --- tests for glep-mode.el  -*-lexical-binding:t-*-

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
(require 'glep-mode)

(defmacro glep-mode-test-run-with-fixed-time (&rest body)
  `(cl-letf* ((fixed-time (date-to-time "2024-08-10 00:00:00 +0000"))
	      (orig-fun (symbol-function 'format-time-string))
	      ((symbol-function 'format-time-string)
	       (lambda (fmt-string &optional time zone)
		 (funcall orig-fun fmt-string (or time fixed-time) zone))))
     ,@body))

(ert-deftest glep-mode-test-font-lock ()
  (with-temp-buffer
    (glep-mode)
    (insert "---\n"
	    "Author: Larry the Cow\n"
	    "---\n"
	    "Author: not highlighted\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "---")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-comment-delimiter-face))
    (search-forward "Author")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-keyword-face))
    (search-forward "Author")
    (should-not (get-text-property (match-beginning 0) 'face))))

(ert-deftest glep-mode-test-update-last-modified ()
  (glep-mode-test-run-with-fixed-time
   (with-temp-buffer
     (insert "---\n"
	     "GLEP: 2\n"
	     "Created: 2003-05-31\n"
	     "Last-Modified: 2023-02-22\n"
	     "---\n")
     (glep-mode-update-last-modified)
     (should (string-equal
	      (buffer-string)
	      (concat "---\n"
		      "GLEP: 2\n"
		      "Created: 2003-05-31\n"
		      "Last-Modified: 2024-08-10\n"
		      "---\n")))
     (erase-buffer)
     (insert "---\n"
	     "GLEP: 2\n"
	     "---\n"
	     ;; Last-Modified outside header must not be touched
	     "Last-Modified: 2023-02-22\n")
     (glep-mode-update-last-modified)
     (should (string-equal
	      (buffer-string)
	      (concat "---\n"
		      "GLEP: 2\n"
		      "---\n"
		      "Last-Modified: 2023-02-22\n"))))))

(ert-deftest glep-mode-test-in-preamble-p ()
  (with-temp-buffer
    (let ((preamble "---\nGLEP: 2\n---\n"))
      (insert preamble "Body text.\n")
      (should (equal
	       (glep-mode-preamble-bounds)
	       (list 1 (length preamble)))))
    (goto-char (point-min))
    (should (glep-mode-in-preamble-p (point)))
    (forward-line 3)
    (should-not (glep-mode-in-preamble-p (point)))))

(provide 'glep-mode-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; glep-mode-tests.el ends here
