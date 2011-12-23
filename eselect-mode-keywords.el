;;; eselect-mode-keywords.el

;; Copyright 2006-2011 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: languages

;; $Id$

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

(defvar eselect-mode-keywords-warn
  '(("eval")
    font-lock-warning-face))

(defvar eselect-mode-keywords-core
  '(("die" "check_do" "do_action" "inherit" "sed")
    font-lock-type-face))

(defvar eselect-mode-keywords-output
  '(("write_error_msg" "write_warning_msg" "write_list_start"
     "write_numbered_list_entry" "write_kv_list_entry" "write_numbered_list"
     "highlight" "highlight_warning" "highlight_marker" "is_output_mode"
     "space")
    font-lock-type-face))

(defvar eselect-mode-keywords-tests
  '(("has" "is_function" "is_number")
    font-lock-type-face))

(defvar eselect-mode-keywords-path-manipulation
  '(("basename" "dirname" "canonicalise" "relative_name")
    font-lock-type-face))

(defvar eselect-mode-keywords-manip
  '(("svn_date_to_version")
    font-lock-type-face))

(defvar eselect-mode-keywords-config
  '(("store_config" "load_config" "append_config")
    font-lock-type-face))

(defvar eselect-mode-keywords-multilib
  '(("list_libdirs")
    font-lock-type-face))

(defvar eselect-mode-keywords-package-manager
  '(("arch" "envvar" "best_version" "has_version" "get_repositories"
     "get_repo_news_dir")
    font-lock-type-face))

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; no-byte-compile: t
;; End:

;;; eselect-mode-keywords.el ends here
