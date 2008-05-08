;;; eselect-mode-keywords.el

;; Copyright 2006-2007 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <opfer@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: languages

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

(defvar eselect-mode-keywords-0
  '("die" "is_function" "has"))

(defvar eselect-mode-keywords-1
  '("store_config" "load_config" "add_config"))

(defvar eselect-mode-keywords-2
  '("svn_date_to_version"))

(defvar eselect-mode-keywords-3
  '("list_libdirs"))

(defvar eselect-mode-keywords-eselect
  '("highlight" "highlight_warning" "space" "write_error_msg"
    "write_kv_list_entry" "write_list_start" "write_numbered_list"
    "write_numbered_list_entry"))

(defvar eselect-mode-keywords-5
  '("is_number" "canonicalise"))

(font-lock-add-keywords
 'eselect-mode
 (mapcar
  '(lambda (x) (apply 'ebuild-mode-make-keywords-list x))
  (list (list eselect-mode-keywords-0 font-lock-type-face)
	(list eselect-mode-keywords-1 font-lock-type-face)
	(list eselect-mode-keywords-2 font-lock-type-face)
	(list eselect-mode-keywords-3 font-lock-warning-face)
	(list eselect-mode-keywords-eselect font-lock-type-face)
	(list eselect-mode-keywords-5 font-lock-type-face))))

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; End:

;;; eselect-mode-keywords.el ends here
