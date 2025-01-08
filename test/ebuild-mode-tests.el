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

(eval-when-compile
  (unless (fboundp 'cl-letf)
    (defalias 'cl-letf  #'letf)
    (defalias 'cl-letf* #'letf*)))

(defmacro ebuild-mode-test-run-with-fixed-time (&rest body)
  (let ((encode-time (if (and (fboundp 'func-arity)
			      (>= 1 (car (func-arity 'encode-time))))
			 ;; new calling convention since Emacs 27
			 '(encode-time) '(apply #'encode-time))))
    `(cl-letf* ((fixed-time (,@encode-time '(0 0 0 10 8 2024 nil nil 0)))
		(orig-fun (symbol-function 'format-time-string))
		((symbol-function 'format-time-string)
		 (lambda (fmt-string &optional time zone)
		   (funcall orig-fun fmt-string (or time fixed-time) zone))))
       ,@body)))

(defmacro ebuild-mode-test-run-silently (&rest body)
  (if (boundp 'inhibit-message)
      `(let ((inhibit-message t)) ,@body)
    `(cl-letf (((symbol-function 'append-message) #'ignore)
	       ((symbol-function 'clear-message) #'ignore))
       ,@body)))

(ert-deftest ebuild-mode-test-arch-lessp ()
  (should (ebuild-mode-arch-lessp "amd64" "x86"))
  (should-not (ebuild-mode-arch-lessp "amd64-linux" "x86"))
  (should (ebuild-mode-arch-lessp "x86-linux" "ppc-macos")))

(ert-deftest ebuild-mode-test-collect-and-split ()
  (let* ((alist '(((a b) z) ((c d) z) ((e) z) ((f) z) ((g h) z)
		  ((i j) y x) ((k) y x) ((l)) ((m) z) ((n o) y x) ((p))
		  ((q r s t u v) w)))
	 (alist1 (copy-tree alist)))
    (should (equal (ebuild-mode-collect-and-split alist)
		   '(((a b c d e f g h m) z) ((i j k n o) y x) ((l p))
		     ((q r s t u v) w))))
    (should (equal (ebuild-mode-collect-and-split alist 4)
		   '(((a b c d) z) ((e f g h) z) ((i j k) y x) ((l p))
		     ((m) z) ((n o) y x) ((q r s t) w) ((u v) w))))
    ;; was it non-destructive?
    (should (equal alist alist1))))

(ert-deftest ebuild-mode-test-font-lock-keywords ()
  (let ((case-fold-search nil)
	(findkey (lambda (key)
		   (catch 'found
		     (dolist (c ebuild-mode-font-lock-keywords)
		       (if (string-match (car c) key)
			   (throw 'found (cdr c))))))))
    ;; Verify that all regexps are below the 32 KiB limit.
    ;; Our regexps are ASCII only, so don't bother with string-bytes
    ;; (GNU Emacs), string-char-byte-conversion-info (XEmacs 21.5),
    ;; or other horrid incompatibilities.
    (should (< (apply #'max (mapcar (lambda (c) (length (car c)))
				    ebuild-mode-font-lock-keywords))
	       32768))
    (should (equal (funcall findkey "doins") 'font-lock-builtin-face))
    (should (equal (funcall findkey "elisp-compile") 'font-lock-type-face))
    (should (equal (funcall findkey "# @ECLASS") '(1 font-lock-type-face t)))
    (should-not (funcall findkey "@ECLASS"))))

(ert-deftest ebuild-mode-test-font-lock ()
  (with-temp-buffer
    (ebuild-mode-test-run-silently
     (ebuild-mode))
    (insert "src_install() {\n"
	    "\temake install\n"
	    "}\n")
    (if (fboundp 'font-lock-ensure)
	(font-lock-ensure)
      (font-lock-fontify-region (point-min) (point-max)))
    (goto-char (point-min))
    (search-forward "src_install")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-type-face))
    (search-forward "emake")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   'font-lock-builtin-face))
    (search-forward "install")
    (should-not (get-text-property (match-beginning 0) 'face))))

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
       (ebuild-mode-update-copyright)
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
	    ((symbol-function 'file-directory-p) #'stringp)
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
	   (ebuild-mode-unescape-string "12\\20034" 'ansi-c)
	   "12\20034"))
  (should (string-equal
	   (decode-coding-string
	    (ebuild-mode-unescape-string "ab\\xc3\\xa4\\xc3\\xb6cd" 'ansi-c)
	    'utf-8-unix)
	   "abäöcd"))
  (should (string-equal
	   (decode-coding-string
	    (ebuild-mode-unescape-string "\\360\\237\\221\\215" 'ansi-c)
	    'utf-8-unix)
	   "👍")))

(ert-deftest ebuild-mode-test-find-s ()
  (cl-letf (((symbol-function 'file-exists-p) #'stringp)
	    ((symbol-function 'file-directory-p) #'stringp)
	    ((symbol-function 'find-file) #'identity)
	    ((symbol-function 'insert-file-contents-literally)
	     (lambda (&rest _args)
	       (insert "declare -x S=\"/tmp/portage/app-misc/foo-1/work/"
		       "foo \xc3\xa4\xe2\x86\x92\\$\\'\\`bar\"\n")
	       (goto-char (point-min))))
	    (ebuild-mode-portage-tmpdir "/tmp/portage")
	    (buffer-file-name
	     "/home/larry/gentoo/app-misc/foo/foo-1.ebuild"))
    (should (equal
	     (ebuild-mode-find-s)
	     "/tmp/portage/app-misc/foo-1/work/foo ä→$\\'`bar"))))

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

(ert-deftest ebuild-mode-test-skeleton ()
  (with-temp-buffer
    (cl-letf* ((testinput
		'("8"			    ; EAPI
		  ""			    ; inherit
		  "Skeleton test"	    ; DESCRIPTION
		  "https://www.gentoo.org/" ; HOMEPAGE
		  ""			    ; SRC_URI
		  ""			    ; S
		  "GPL-2+" "MIT" ""	    ; LICENSE
		  "~amd64" ""		    ; KEYWORDS
		  ""			    ; IUSE
		  ""))			    ; RESTRICT
	       (getinput (lambda (&rest _args)
			   (concat (pop testinput))))
	       ((symbol-function 'read-from-minibuffer) getinput)
	       ((symbol-function 'read-string) getinput)
	       ;; prevent a segfault (seen with XEmacs 21.4.24 and 21.5.35)
	       ;; https://foss.heptapod.net/xemacs/xemacs/-/issues/6
	       ((symbol-function 'pos-visible-in-window-p)
		(lambda (&rest _args) t)))
      (ebuild-mode-test-run-with-fixed-time
       (ebuild-mode-insert-skeleton)))
    (should (string-equal
	     (buffer-string)
	     (concat "# Copyright 2024 Gentoo Authors\n"
		     "# Distributed under the terms of the "
		     "GNU General Public License v2\n\n"
		     "EAPI=8\n\n"
		     "DESCRIPTION=\"Skeleton test\"\n"
		     "HOMEPAGE=\"https://www.gentoo.org/\"\n"
		     "SRC_URI=\"\"\n\n"
		     "LICENSE=\"GPL-2+ MIT\"\n"
		     "SLOT=\"0\"\n"
		     "KEYWORDS=\"~amd64\"\n\n"
		     "RDEPEND=\"\"\n"
		     "DEPEND=\"${RDEPEND}\"\n"
		     "BDEPEND=\"\"\n")))))

(ert-deftest ebuild-mode-test-bug-url ()
  (skip-unless (fboundp 'bug-reference-prog-mode))
  (let* ((ebuild-mode-enable-bug-reference t)
	 found
	 (browse-url-browser-function
	  (lambda (url &rest _args) (setq found url))))
    (with-temp-buffer
      (insert "# abc #876543 xyz\n"
	      "# bug 765432\n"
	      "# bug #654321#c10\n")
      (ebuild-mode-test-run-silently
       (ebuild-mode))
      (bug-reference-fontify (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "#" nil nil 2)
      (bug-reference-push-button (point))
      (should (equal found "https://bugs.gentoo.org/876543"))
      (setq found nil)
      (search-forward "bug")
      (bug-reference-push-button (point))
      (should (equal found "https://bugs.gentoo.org/765432"))
      (setq found nil)
      (search-forward "bug")
      (bug-reference-push-button (point))
      (should (equal found "https://bugs.gentoo.org/654321#c10"))
      (setq found nil)
      (bug-reference-push-button (point-min))
      (bug-reference-push-button (point-max))
      (should-not found))))

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

(ert-deftest ebuild-mode-test-keybindings ()
  (should (equal (lookup-key ebuild-mode-map "\C-c\C-e\C-k")
		 'ebuild-mode-keyword))
  (should (equal (lookup-key ebuild-repo-mode-map "\C-c-")
		 'ebuild-mode-insert-tag-line))
  (with-temp-buffer
    (ebuild-mode-test-run-silently
     (ebuild-mode))
    (should (equal (local-key-binding "\C-c\C-eu")
		   'ebuild-run-command-unpack))))

(provide 'ebuild-mode-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ebuild-mode-tests.el ends here
