;;; xemacs-test-wrapper.el --- ERT test wrapper for XEmacs

;; Copyright 2024-2025 Gentoo Authors

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
;; along with XEmacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Quick and dirty hack to make the tests work with XEmacs, where ERT
;; is not available. It defines some macros (just the few that we need;
;; this is far from being complete) in terms of the XEmacs test suite
;; harness.

;; Run the tests:
;; xemacs -batch -q -no-site-file -eval "(add-to-list 'load-path nil)" \
;; -l test/xemacs-test-wrapper -f batch-test-emacs test/mytest.el

;;; Code:

(require 'test-harness)
(provide 'ert)				; pretend that ERT is present

(define-error 'test-skipped "Test skipped")

(defmacro ert-deftest (name _args &rest body)
  `(condition-case nil
       ;; XEmacs 21.5.35 displays random info-level warnings,
       ;; e.g. for "format" called with too many arguments.
       ;; Increase the minimum level to silence them.
       (let ((log-warning-minimum-level 'notice))
	 ,@body)
     (test-skipped (message "SKIP: %s" ',name))))

(defun skip-unless (cond)
  (unless cond (signal 'test-skipped nil)))

(defmacro should (assertion)
  (let ((args (ignore-errors
		(destructuring-bind (s1 (s2 form) (s3 (err msg)))
		    assertion
		  (list (list s1 s2 s3) err msg form)))))
    ;; handle (should (equal (should-error ...) '(error ...)))
    (if (equal (car args) '(equal should-error quote))
	`(Check-Error-Message ,@(cdr args))
      `(Assert ,assertion))))

(defmacro should-not (assertion)
  `(Assert (not ,assertion)))

(defmacro should-error (form)
  `(Check-Error 'error ,form))

;; return a useful exit status
(defadvice kill-emacs (before xemacs-test-wrapper-kill-emacs activate)
  (let ((ret (ad-get-arg 0)))
    (cond ((and (integerp ret) (>= ret 2)))
	  ((/= unexpected-test-suite-failures 0)
	   (setq ret 2))
	  (t (dolist (result test-harness-file-results-alist)
	       ;; result is a list: (file passes total)
	       (if (/= (nth 1 result) (nth 2 result))
		   (setq ret 1)))))
    (ad-set-arg 0 ret)))

;;; xemacs-test-wrapper.el ends here
