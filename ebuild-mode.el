;;; ebuild-mode.el --- a mode for editing Portage .ebuild, .eclass and .eselect files.

;; Copyright (C) 2003-2007  Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;; Author: Diego Pettenò <flameeyes@gentoo.org>
;; Author: Christian Faulhammer <opfer@gentoo.org>
;; Author: Ulrich Müller <ulm@gentoo.org>
;; Keywords: convenience
;; version: 1.6
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The commands have been grouped into lists of source (eclass).
;; We map each set of keywords to the basic faces: font-lock-*-face.

;;; Code:

(defvar ebuild-mode-hook nil
  "List of functions to call when entering ebuild-mode")

(defvar ebuild-mode-commands-0
  '("best_version" "built_with_use" "check_KV" "die" "diropts" "dobin"
    "docinto" "doconfd" "dodir" "dodoc" "doenvd" "doexe" "dohard" "dohtml"
    "doicon" "doinfo" "doinitd" "doins" "dojar" "dolib" "dolib.a" "dolib.so"
    "doman" "domenu" "domo" "dopython" "dosbin" "dosed" "dosym" "ebeep"
    "ebegin" "econf" "eend" "eerror" "einfo" "einstall" "elog" "emake"
    "enewgroup" "enewuser" "epatch" "epause" "ewarn" "exeinto" "exeopts"
    "fowners" "fperms" "has" "has_version" "hasq" "hasv" "insinto" "insopts"
    "into" "keepdir" "libopts" "make_desktop_entry" "newbin" "newconfd"
    "newdoc" "newenvd" "newexe" "newinitd" "newins" "newlib.a" "newlib.so"
    "newman" "newsbin" "prepall" "prepalldocs" "prepallinfo" "prepallman"
    "prepallstrip" "unpack" "use" "use_enable" "use_with" "useq" "usev"))

(defvar ebuild-mode-commands-sandbox
  '("adddeny" "addpredict" "addread" "addwrite"))

(defvar ebuild-mode-commands-eclass
  '("inherit"))

;; functions from flag-o-matic eclass
(defvar ebuild-mode-commands-flag-o-matic
  '("append-flags" "append-ldflags" "filter-flags" "filter-ldflags"
    "filter-mfpmath" "get-flag" "is-flag" "replace-cpu-flags" "replace-flags"
    "strip-flags" "strip-unsupported-flags"))

;; functions from elisp-common eclass
(defvar ebuild-mode-commands-elisp
  '("elisp-comp" "elisp-compile" "elisp-install" "elisp-site-file-install"
    "elisp-site-regen" "elisp-emacs-version" "elisp-make-autoload-file"))

(defun ebuild-mode-make-keywords-list (keywords-list face &optional prefix suffix)
  ;; based on `generic-make-keywords-list' from generic.el
  (unless (listp keywords-list)
    (error "Keywords argument must be a list of strings"))
  (cons (concat prefix "\\<"
		(regexp-opt keywords-list t)
		"\\>" suffix)
	face))

(font-lock-add-keywords
 'ebuild-mode
 (list (ebuild-mode-make-keywords-list ebuild-mode-commands-0 'font-lock-type-face)
       (ebuild-mode-make-keywords-list ebuild-mode-commands-sandbox 'font-lock-warning-face)
       (ebuild-mode-make-keywords-list ebuild-mode-commands-eclass 'font-lock-type-face)
       (ebuild-mode-make-keywords-list ebuild-mode-commands-flag-o-matic 'font-lock-type-face)
       (ebuild-mode-make-keywords-list ebuild-mode-commands-elisp 'font-lock-type-face)))

(defun ebuild-mode-tabify ()
  ;; tabify whitespace only at beginning of lines
  (let ((tabify-regexp "^\t* [ \t]+"))
    (tabify (point-min) (point-max))))

(define-derived-mode ebuild-mode shell-script-mode "Ebuild"
  "Major mode for Portage .ebuild and .eclass files."
  (add-hook 'write-contents-hooks 'delete-trailing-whitespace t t)
  (add-hook 'write-contents-hooks 'ebuild-mode-tabify t t)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  ;; run user-defined hooks
  (run-hooks 'ebuild-mode-hook))

(defvar eselect-mode-commands-0
  '("die" "is_function" "has"))

(defvar eselect-mode-commands-1
  '("store_config" "load_config" "add_config"))

(defvar eselect-mode-commands-2
  '("svn_date_to_version"))

(defvar eselect-mode-commands-3
  '("list_libdirs"))

(defvar eselect-mode-commands-4
  '("highlight" "highlight_warning" "space" "write_error_msg"
    "write_kv_list_entry" "write_list_start" "write_numbered_list"
    "write_numbered_list_entry"))

(defvar eselect-mode-commands-5
  '("is_number canonicalise"))

(font-lock-add-keywords
 'eselect-mode
 (list (ebuild-mode-make-keywords-list eselect-mode-commands-0 'font-lock-type-face)
       (ebuild-mode-make-keywords-list eselect-mode-commands-1 'font-lock-type-face)
       (ebuild-mode-make-keywords-list eselect-mode-commands-2 'font-lock-type-face)
       (ebuild-mode-make-keywords-list eselect-mode-commands-3 'font-lock-warning-face)
       (ebuild-mode-make-keywords-list eselect-mode-commands-4 'font-lock-type-face)
       (ebuild-mode-make-keywords-list eselect-mode-commands-5 'font-lock-type-face)))

(define-derived-mode eselect-mode shell-script-mode "Eselect"
  "Major mode for Portage .eselect files."
  (add-hook 'write-contents-hooks 'delete-trailing-whitespace t t)
  (add-hook 'write-contents-hooks 'ebuild-mode-tabify t t)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(defvar ebuild-commands-alist
  (mapcar 'list
	  '("help" "setup" "fetch" "digest" "manifest" "unpack" "compile"
	    "test" "preinst" "postinst" "install" "qmerge" "merge"
	    "prerm" "postrm" "unmerge" "config" "package" "rpm" "clean")))

(defun ebuild-run-command (command)
  "Run ebuild COMMAND, with output to a compilation buffer."
  (interactive
   (list (completing-read
	  "Run ebuild command: " ebuild-commands-alist nil t)))
  (or (assoc command ebuild-commands-alist)
      (error "Ebuild command \"%s\" not known" command))
  (let ((process-environment
	 (cons "NOCOLOR=true" process-environment))
	;;(compilation-mode-hook
	;; (function (lambda () (setq truncate-lines t))))
	(compilation-buffer-name-function
	 (list 'lambda '(mode) (concat "*ebuild " command "*"))))
    (compile (format "ebuild %s %s" buffer-file-name command))))

(define-key ebuild-mode-map "\C-ce" 'ebuild-run-command)

(and (< emacs-major-version 22)
     ;; make TAB key work
     (defadvice sh-must-be-shell-mode
       (around ebuild-mode-sh-must-be-shell-mode activate)
       (or (memq major-mode '(ebuild-mode eselect-mode))
	   ad-do-it)))

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist '("\\.eselect\\'" . eselect-mode))

(provide 'ebuild-mode)

;;; ebuild-mode.el ends here
