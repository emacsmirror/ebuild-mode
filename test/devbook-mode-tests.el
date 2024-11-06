;;; devbook-mode-tests.el --- tests for devbook-mode.el -*-lexical-binding:t-*-

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
(require 'devbook-mode)

(defmacro devbook-mode-test-run-silently (&rest body)
  `(let ((inhibit-message t)) ,@body))

(ert-deftest devbook-mode-test-set-schema ()
  (cl-letf* ((rncfile "/home/larry/devmanual/devbook.rnc")
	     (rncschema "start = element foo { empty }\n")
	     ((symbol-function 'file-exists-p)
	      (lambda (file) (string-equal file rncfile)))
	     ((symbol-function 'file-directory-p)
	      (lambda (_file) t))
	     ((symbol-function 'insert-file-contents)
	      (lambda (file &rest _args)
		(unless (string-equal file rncfile)
		  (signal 'file-missing nil))
		(insert rncschema))))
    (let ((buffer-file-name "/home/larry/devmanual/quickstart/text.xml"))
      (should (equal (devbook-set-schema) rncfile))
      (setq rncschema "foo = element foo { empty }\n") ; bad schema
      (should (equal
	       (car (should-error (devbook-set-schema)))
	       'rng-c-incorrect-schema)))
    (let ((buffer-file-name "/home/larry/elsewhere/text.xml"))
      (should-not (devbook-set-schema)))))

(ert-deftest devbook-mode-test-skeleton ()
  (with-temp-buffer
    (cl-letf* ((buffer-file-name "/home/larry/devmanual/quickstart/text.xml")
	       (testinput '("Quickstart guide"))
	       (getinput (lambda (&rest _args)
			   (concat (pop testinput))))
	       ((symbol-function 'read-from-minibuffer) getinput)
	       ((symbol-function 'read-string) getinput))
      (devbook-insert-skeleton))
    (let ((buf1 (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			"<guide self=\"quickstart/\">\n"
			"<chapter>\n"
			"<title>Quickstart guide</title>\n"))
	  (buf2 (concat "\n"
			"</chapter>\n"
			"</guide>\n")))
      (should (equal (point)
		     (+ (point-min) (length buf1))))
      (should (string-equal (buffer-string)
			    (concat buf1 buf2))))))

(ert-deftest devbook-mode-test-keybindings ()
  (should (equal (lookup-key devbook-mode-map "\C-c\C-n")
		 'devbook-insert-skeleton))
  (with-temp-buffer
    (devbook-mode-test-run-silently
     (devbook-mode))
    (should (equal (local-key-binding "\C-c\C-n")
		   'devbook-insert-skeleton))))

(provide 'devbook-mode-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; devbook-mode-tests.el ends here
