;;; ebuild-mode-tests.el --- tests for ebuild-mode.el  -*-lexical-binding:t-*-

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
(require 'ebuild-mode)

(defmacro ebuild-mode-test-run-with-fixed-time (&rest body)
  `(cl-letf* ((fixed-time (date-to-time "2024-08-10T00:00:00Z"))
	      (orig-fun (symbol-function 'format-time-string))
	      ((symbol-function 'format-time-string)
	       (lambda (fmt-string &optional time &rest args)
		 (apply orig-fun fmt-string (or time fixed-time) args))))
     ,@body))

(ert-deftest ebuild-mode-test-arch-lessp ()
  (should (ebuild-mode-arch-lessp "amd64" "x86"))
  (should-not (ebuild-mode-arch-lessp "amd64-linux" "x86"))
  (should (ebuild-mode-arch-lessp "x86-linux" "ppc-macos")))

(ert-deftest ebuild-mode-test-time-string ()
  (should (string-equal
	   (ebuild-mode-time-string "%Y-%m-%d %H:%M:%S" '(14257 22633))
	   "1999-08-11 11:03:05"))
  (should (string-equal
	   (ebuild-mode-time-string "%Y-%m-%d %H:%M:%S" '(33451 44363))
	   "2039-06-21 17:11:39")))

(ert-deftest ebuild-mode-test-font-lock ()
  (with-temp-buffer
    (let ((inhibit-message t))
      (ebuild-mode))
    (insert "src_install() {\n"
	    "\temake install\n"
	    "}\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "src_install")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-type-face))
    (search-forward "emake")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-builtin-face))
    (search-forward "install")
    (should-not (get-text-property (match-beginning 0) 'face))))

(ert-deftest ebuild-mode-test-collect-and-split ()
  (let* ((alist '(((a b) z) ((c d) z) ((e) z) ((f) z) ((g h) z)
		  ((i j) y x) ((k) y x) ((l)) ((m) z) ((n o) y x) ((p))
		  ((q r s t u v) w)))
	 (alist-copy (copy-tree alist)))
    (should (equal (ebuild-mode-collect-and-split alist)
		   '(((q r s t u v) w) ((l p)) ((i j k n o) y x)
		     ((a b c d e f g h m) z))))
    (should (equal (ebuild-mode-collect-and-split alist 4)
		   '(((q r) w) ((s t u v) w) ((n o) y x) ((m) z) ((l p))
		     ((i j k) y x) ((e f g h) z) ((a b c d) z))))
    ;; was it non-destructive?
    (should (equal alist alist-copy))))

(ert-deftest ebuild-mode-test-update-copyright ()
  (let ((ebuild-mode-update-copyright t))
    (ebuild-mode-test-run-with-fixed-time
     (with-temp-buffer
       (insert "# Copyright 2023 Gentoo Foundation\n")
       (ebuild-mode-update-copyright)
       (should (string-equal
		(buffer-string)
		"# Copyright 2023-2024 Gentoo Authors\n"))
       (erase-buffer)
       (insert "# Copyright 2020-2023 other author\n")
       (should (string-equal
		(buffer-string)
		"# Copyright 2020-2023 other author\n"))))))

(ert-deftest ebuild-mode-test-delete-cvs-line ()
  (with-temp-buffer
    (insert "# Copyright 2024 Gentoo Authors\n"
	    "# $Id$\n")
    (ebuild-mode-delete-cvs-line)
    (should (string-equal
	     (buffer-string)
	     "# Copyright 2024 Gentoo Authors\n"))))

(ert-deftest ebuild-mode-test-get-builddir ()
  (cl-letf (((symbol-function 'file-exists-p)
	     (lambda (file)
	       (string-equal file "/home/larry/gentoo/profiles/repo_name")))
	    ((symbol-function 'file-directory-p) (lambda (_file) t))
	    (ebuild-mode-portage-tmpdir "/var/tmp/portage"))
    (let ((buffer-file-name
	   "/home/larry/gentoo/app-editors/emacs/emacs-29.4.ebuild"))
      (should (equal
	       (ebuild-mode-get-builddir)
	       "/var/tmp/portage/app-editors/emacs-29.4")))
    (let ((buffer-file-name "/home/larry/emacs-29.4.ebuild"))
      (should (equal
	       (should-error (ebuild-mode-get-builddir))
	       '(error "This does not look like an ebuild repository"))))
    (let ((buffer-file-name "/home/larry/gentoo/eclass/elisp.eclass"))
      (should-error (ebuild-mode-get-builddir)))))

(ert-deftest ebuild-mode-test-unescape-string ()
  (should (string-equal
	   (ebuild-mode-unescape-string "ab\\\"\\$\\\\cd\\\n\\ef\\")
	   "ab\"$\\cd\n\\ef\\"))
  (should (string-equal
	   (ebuild-mode-unescape-string "ab\\r\\E\\'\\143\\144\\ ef" 'ansi-c)
	   "ab\r\e'cd\\ ef"))
  (should (string-equal
	   (ebuild-mode-unescape-string "äöü" 'ansi-c)
	   "äöü"))
  (should (string-equal
	   (decode-coding-string
	    (ebuild-mode-unescape-string "\\360\\237\\221\\215" 'ansi-c)
	    'utf-8-unix)
	   "👍")))

(ert-deftest ebuild-mode-test-get-keywords ()
  (with-temp-buffer
    (insert "KEYWORDS=\"amd64 arm ~ppc64 x86\"\n")
    (should (equal
	     (ebuild-mode-get-keywords 'noerror)
	     '(("amd64" . "") ("arm" . "") ("ppc64" . "~") ("x86" . ""))))
    (erase-buffer)
    (should (equal
	     (should-error (ebuild-mode-get-keywords))
	     '(error "No KEYWORDS assignment found")))
    (insert "\tKEYWORDS=\"amd64\"\n"
	    "\tKEYWORDS=\"\"\n")
    (should (equal
	     (should-error (ebuild-mode-get-keywords))
	     '(error "More than one KEYWORDS assignment found")))))

(ert-deftest ebuild-mode-test-put-keywords ()
  (with-temp-buffer
    (insert "KEYWORDS=\"~amd64 ~ppc\"\n")
    (ebuild-mode-put-keywords
     '(("amd64" . "") ("arm" . "") ("ppc64" . "~") ("x86" . "")) 'noerror)
    (should (string-equal
	     (buffer-string)
	     "KEYWORDS=\"amd64 arm ~ppc64 x86\"\n"))
    (erase-buffer)
    (should-error (ebuild-mode-put-keywords '(("amd64" . ""))))))

(ert-deftest ebuild-mode-test-modify-keywords ()
  (with-temp-buffer
    (insert "KEYWORDS=\"x86 ~ppc64 ~amd64 arm\"\n")
    (ebuild-mode-modify-keywords
     '(("arm64" . "~") ("amd64" . "") ("*" . "-")))
    (should (string-equal
	     (buffer-string)
	     "KEYWORDS=\"-* amd64 arm ~arm64 ~ppc64 x86\"\n"))))

(ert-deftest ebuild-mode-test-all-keywords-unstable ()
  (with-temp-buffer
    (insert "KEYWORDS=\"amd64 x86 arm -m68k ~ppc64\"\n")
    (ebuild-mode-all-keywords-unstable)
    (should (string-equal
	     (buffer-string)
	     "KEYWORDS=\"~amd64 ~arm -m68k ~ppc64 ~x86\"\n"))))

(ert-deftest ebuild-mode-test-insert-tag-line ()
  (let ((ebuild-mode-full-name "Larry the Cow")
	(ebuild-mode-mail-address "larry@example.org"))
    (ebuild-mode-test-run-with-fixed-time
     (with-temp-buffer
       (let ((comment-start nil))
	 (ebuild-mode-insert-tag-line))
       (should (string-equal
		(buffer-string)
		"Larry the Cow <larry@example.org> (2024-08-10)\n"))
       (erase-buffer)
       (let ((comment-start "#"))
	 (ebuild-mode-insert-tag-line))
       (should (string-equal
		(buffer-string)
		"# Larry the Cow <larry@example.org> (2024-08-10)\n"))))))

(provide 'ebuild-mode-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ebuild-mode-tests.el ends here
