;;; ebuild-mode.el --- a mode for editing Portage .ebuild, .eclass and .eselect files.

;; Copyright (C) 2003-2007  Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;; Author: Diego Pettenò <flameeyes@gentoo.org>
;; Author: Christian Faulhammer <opfer@gentoo.org>
;; Keywords: convenience
;; version: 1.4
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

;; The commands have been grouped into lists of rough similarity.  If
;; you can think of a better way to arrange these, please let us know.
;; We map each set of keywords to the basic faces: font-lock-*-face.

;; (add-to-list 'auto-mode-alist '("\\.\\(ebuild\\|eclass\\)\\'" . ebuild-mode))
;; (add-to-list 'auto-mode-alist '("\\.eselect\\'" . eselect-mode))

;;; Code:

(defvar ebuild-mode-hook nil
  "List of functions to call when entering ebuild-mode")


(defvar ebuild-mode-commands-0
  '("use" "has_version" "best_version" "use_with" "use_enable" "check_KV" "keepdir" "econf" "die" "eerror" "einstall" "einfo" "elog" "ewarn" "diropts" "dobin" "docinto" "dodoc" "doexe" "dohard" "dohtml" "doinfo" "doins" "dolib" "dolib.a" "dolib.so" "doman" "dosbin" "dosym" "emake" "exeinto" "exeopts" "fowners" "fperms" "insinto" "insopts" "into" "libopts" "newbin" "newexe" "newins" "newman" "newsbin" "prepall" "prepalldocs" "prepallinfo" "prepallman" "prepallstrip" "has" "unpack" "dopython" "dosed" "into" "doinitd" "doconfd" "doenvd" "dojar" "domo" "dodir" "ebegin" "eend" "newconfd" "newdoc" "newenvd" "newinitd" "newlib.a" "newlib.so" "hasq" "hasv" "useq" "usev" "epause" "ebeep" "epatch" "enewuser" "enewgroup" "make_desktop_entry" "domenu" "doicon" "built_with_use"))

(defvar ebuild-mode-commands-1
  '("addread" "addwrite" "adddeny" "addpredict"))

(defvar ebuild-mode-commands-2
  '("inherit"))

(defvar ebuild-mode-commands-3
 '("filter-flags" "append-flags" "replace-flags" "replace-cpu-flags" "is-flag" "strip-flags" "strip-unsupported-flags" "get-flag" "filter-mfpmath" "append-ldflags" "filter-ldflags"))

(defvar ebuild-mode-commands-4
 '("elisp-compile" "elisp-install" "elisp-site-file-install" "elisp-site-regen" "elisp-comp"))

(defun ebuild-mode-make-keywords-list (keywords-list face &optional prefix suffix)
  ;; based on `make-generic-keywords-list' from generic.el
  (unless (listp keywords-list)
    (error "Keywords argument must be a list of strings"))
  (cons (concat prefix "\\<"
		(regexp-opt keywords-list t)
		"\\>" suffix)
	face))

(defun ebuild-mode-tabify ()
  (save-excursion 
    (tabify (point-min) (point-max))))

(define-derived-mode ebuild-mode shell-script-mode "Ebuild"
  "Major mode for Portage .ebuild and .eclass files."
  (font-lock-add-keywords 'ebuild-mode
   (list (ebuild-mode-make-keywords-list ebuild-mode-commands-0 'font-lock-type-face)
         (ebuild-mode-make-keywords-list ebuild-mode-commands-1 'font-lock-warning-face)
         (ebuild-mode-make-keywords-list ebuild-mode-commands-2 'font-lock-warning-face)
	 (ebuild-mode-make-keywords-list ebuild-mode-commands-3 'font-lock-type-face)))
  (add-hook 'write-file-functions 'delete-trailing-whitespace t t)
  (add-hook 'write-file-functions 'ebuild-mode-tabify t t)
  (setq tab-width 4
        indent-tabs-mode t)
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
  '("write_error_msg" "write_list_start" "write_kv_list_entry" "write_numbered_list_entry" "write_numbered_list" "highlight" "highlight_warning" "space"))

(defvar eselect-mode-commands-5
  '("is_number canonicalise"))

(define-derived-mode eselect-mode shell-script-mode "Eselect"
  "Major mode for Portage .eselect files."
  (font-lock-add-keywords 'eselect-mode
   (list (ebuild-mode-make-keywords-list eselect-mode-commands-0 'font-lock-type-face)
	 (ebuild-mode-make-keywords-list eselect-mode-commands-1 'font-lock-type-face)
	 (ebuild-mode-make-keywords-list eselect-mode-commands-2 'font-lock-type-face)
	 (ebuild-mode-make-keywords-list eselect-mode-commands-3 'font-lock-warning-face)
	 (ebuild-mode-make-keywords-list eselect-mode-commands-4 'font-lock-type-face)
	 (ebuild-mode-make-keywords-list eselect-mode-commands-5 'font-lock-type-face)))
  (add-hook 'write-file-functions 'delete-trailing-whitespace t t)
  (add-hook 'write-file-functions 'ebuild-mode-tabify t t)
  (setq tab-width 4
        indent-tabs-mode t))

(defun ebuild-mode-run-command (command)
  (let ((process-connection-type "t")
	(buffer (format "*ebuild %s*" command)))
    (start-process "ebuild-digest" buffer "env" "NOCOLOR=yes" "ebuild" (buffer-file-name) command)
    (pop-to-buffer buffer)))

(defmacro define-ebuild-mode-command (key command)
  (let ((name (intern (format "ebuild-mode-command-%s" command))))
    `(progn
       (defun ,name ()
	 ,(format "Runs the ebuild %s command for the ebuild in the current buffer" command)
	 (interactive)
	 (ebuild-mode-run-command ,command))
       (define-key ebuild-mode-map ,key ',name))))

(define-ebuild-mode-command "\C-ced" "digest")
(define-ebuild-mode-command "\C-cef" "fetch")
(define-ebuild-mode-command "\C-ceu" "unpack")

(provide 'ebuild-mode)

;;; ebuild-mode.el ends here
