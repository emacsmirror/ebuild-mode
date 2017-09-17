;;; glep-mode.el --- edit Gentoo Linux Enhancement Proposals

;; Copyright 2017 Gentoo Foundation

;; Author: Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: languages

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, version 3 of the License.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; This file contains in "glep-mode-insert-skeleton" some adapted
;; material taken from GLEP 2, Sample ReStructuredText GLEP Template,
;; Grant Goodyear, Chris Reffett, Ulrich Müller, originally licensed
;; under the Creative Commons Attribution-ShareAlike 3.0 License.
;; By section 4(b) of that license, the adaptation can be distributed
;; under CC BY-SA version 4.0.  It is in turn included here under
;; the terms of the GPLv3, via the one-way compatibility approved in
;; https://creativecommons.org/compatiblelicenses.

;; By section 14 of the GPLv3, Creative Commons is hereby specified
;; as a proxy (via http://creativecommons.org/compatiblelicenses)
;; to decide whether future versions of the GPL can be used for this
;; work.

;;; Commentary:

;;; Code:

(require 'rst)
(require 'font-lock)
(require 'easymenu)
(require 'skeleton)

(defvar glep-mode-font-lock-keywords
  (eval-when-compile
    (concat "^"
	    (regexp-opt
	     '("GLEP" "Title" "Author" "Type" "Status" "Version"
	       "Created" "Last-Modified" "Post-History" "Content-Type"
	       "Requires" "Replaces" "Replaced-By")
	     t)
	    ":"))
  "Expressions to highlight in the preamble of a GLEP.")

(defvar glep-mode-delim-re "^---$"
  "Regexp matching delimiters of the GLEP header.")

(defvar glep-mode-preamble-limit 2000
  "Maximum length of GLEP preamble.
For efficiency only. Unlimited if nil.")

;;;###autoload
(define-derived-mode glep-mode rst-mode "GLEP"
  "Major mode for Gentoo Linux Enhancement Proposals."
  ;; (setq indent-tabs-mode nil)
  ;; Tabs are allowed by GLEP 2, and should represent 4 spaces.
  ;; (Whereas PEP 12 said: "Tab characters must never appear
  ;; in the document at all.")
  (setq tab-width 4)
  ;; GLEP 2 specifies double space after a full stop, and filling
  ;; of paragraphs to column 70.
  (setq sentence-end-double-space t)
  (setq fill-column 70)
  (add-hook 'font-lock-extend-region-functions
	    'glep-mode-font-lock-extend-region t))

(add-hook
 'glep-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil `((glep-mode-font-lock-match-delims
	   . ,font-lock-comment-delimiter-face)
	  (glep-mode-font-lock-match-preamble
	   . ,font-lock-keyword-face)))))

(defun glep-mode-preamble-bounds ()
  "Return list with begin and end of the preamble, or nil if none found."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (and (re-search-forward glep-mode-delim-re
			      glep-mode-preamble-limit t)
	   (let ((beg (match-beginning 0)))
	     (if (re-search-forward glep-mode-delim-re
				    glep-mode-preamble-limit t)
		 (list beg (match-end 0))))))))

(defun glep-mode-in-preamble-p (pos)
  "Return non-nil if position POS is inside the GLEP's preamble."
  (let ((pre (glep-mode-preamble-bounds)))
    (and pre (>= pos (car pre)) (<= pos (cadr pre)))))

(defun glep-mode-font-lock-match-delims (limit)
  "Match delimiters to be highlighted by font-lock."
  (let ((pos (save-excursion
	       (re-search-forward glep-mode-delim-re limit t))))
    (and pos
	 (glep-mode-in-preamble-p pos)
	 (goto-char pos))))

(defun glep-mode-font-lock-match-preamble (limit)
  "Match expressions in the preamble to be highlighted by font-lock."
  (let ((pos (save-excursion
	       (re-search-forward glep-mode-font-lock-keywords limit t))))
    (and pos
	 (glep-mode-in-preamble-p pos)
	 (goto-char pos))))

(defun glep-mode-font-lock-extend-region ()
  "Extend the font-lock region if it might be in a multi-line construct.
Return non-nil if so.  Font-lock region is from `font-lock-beg'
to `font-lock-end'."
  (let ((pre (glep-mode-preamble-bounds))
	ret)
    (when pre
      (and (> font-lock-beg (car pre))
	   (<= font-lock-beg (cadr pre))
	   (setq font-lock-beg (car pre)
		 ret t))
      (and (>= font-lock-end (car pre))
	   (< font-lock-end (cadr pre))
	   (setq font-lock-end (cadr pre)
		 ret t))
      ret)))

;; Prevent rst-mode from interpreting the "---" delimiter as section header.
;; We cannot use advice-add because it did not exist in Emacs 23.
;; *** FIXME *** This is incomplete and probably too brittle.
(defadvice rst-classify-adornment (around glep-ignore-preamble)
  "Ignore GLEP preamble in rst-classify-adornment."
  (if (not (and (eq major-mode 'glep-mode)
		(glep-mode-in-preamble-p (ad-get-arg 1))))
      ad-do-it))

(ad-activate 'rst-classify-adornment)

;;; Skeleton support.

;; Header format, as specified by GLEP 1.
;;   ---
;;   GLEP: <glep number>
;;   Title: <glep title>
;;   Author: <list of authors' real names and optionally, email addrs>
;;   Type: <Informational | Standards Track>
;;   Status: <Draft | Active | Accepted | Deferred | Withdrawn | Rejected |
;;            Final | Replaced | Moribund>
;;   Version: <major>[.<minor>]
;;   Created: <date created on>
;;   Last-Modified: <date of last update>
;;   Post-History: <dates of postings to mailing lists>
;;   Content-Type: <text/x-rst>
;; * Requires: <glep numbers>
;; * Replaces: <glep number>
;; * Replaced-By: <glep number>
;;   ---

(define-skeleton glep-mode-insert-skeleton
  "Insert a skeleton for a Gentoo Linux Enhancement Proposal."
  nil
  "---\n"
  "GLEP: " (skeleton-read "GLEP: " "XXX") "\n"
  "Title: " (skeleton-read "Title: ") "\n"
  "Author: " (skeleton-read
	      "Author's real name and e-mail address: "
	      (concat user-full-name " <" user-mail-address ">"))
  "\n"
  "Type: "
  (completing-read
   "Type (TAB for completion): "
   (mapcar 'list '("Informational" "Standards Track"))
   nil 'confirm)
  "\n"
  "Status: "
  (completing-read
   "Status (TAB for completion): "
   (mapcar 'list '("Draft" "Active" "Accepted" "Deferred" "Withdrawn"
		   "Rejected" "Final" "Replaced" "Moribund"))
   nil 'confirm "Draft")
  "\n"
  "Version: " (skeleton-read "Version: " "1") "\n"
  "Created: " (format-time-string "%Y-%m-%d") "\n"
  "Last-Modified: " (format-time-string "%Y-%m-%d") "\n"
  "Post-History: \n"
  ((skeleton-read "Requires: (null string if none): ") "Requires: " str "\n")
  ((skeleton-read "Replaces: (null string if none): ") "Replaces: " str "\n")
  "---\n"
  "\n\n"
  ;; Most of the following text is taken from GLEP 2.
  "\
How to Use This Template
========================

To use this template you must first decide whether your GLEP is going to be an
Informational or Standards Track GLEP.  Most GLEPs are Standards Track because
they propose new functionality for some aspect of Gentoo Linux.  When in
doubt, read GLEP 1 for details or contact the GLEP editors <glep@gentoo.org>.

Once you've decided which type of GLEP yours is going to be, follow the
directions below.

- Change the Title header to the title of your GLEP.

- Change the Author header to include your name, and optionally your e-mail
  address.  Be sure to follow the format carefully: your name must appear
  first, and it must not be contained in parentheses.  Your e-mail address
  may appear second (or it can be omitted) and if it appears, it must appear
  in angle brackets.

- For Standards Track GLEPs, change the Type header to \"Standards Track\".

- For Informational GLEPs, change the Type header to \"Informational\".

- Change the Status header to \"Draft\".

- Reset the Version to \"1\".

- Change the Created and Last-Modified headers to today's date.  Be sure to
  follow the format carefully: it must be in ISO 8601 ``yyyy-mm-dd`` format.

- Reset the Post-History to empty for now; you'll add dates to this header
  each time you post your GLEP to gentoo-dev@lists.gentoo.org.  If you
  posted your GLEP to the list on August 14, 2003 and September 3, 2003,
  the Post-History header would look like::

      Post-History: 2003-08-14, 2003-09-03

  You must manually add new dates and check them in.  If you don't have
  check-in privileges, send your changes to the GLEP editors.

- For Standards Track GLEPs, if your feature depends on the acceptance
  of some other currently in-development GLEP, add a Requires header right
  after the Type header.  The value should be the GLEP number of the GLEP
  yours depends on.  Don't add this header if your dependent feature is
  described in a Final GLEP.

- Add a Replaces header if your GLEP obsoletes an earlier GLEP.  The value
  of this header is the number of the GLEP that your new GLEP is replacing.
  Only add this header if the older GLEP is in \"final\" form, i.e. is either
  Accepted, Final, or Rejected.  You aren't replacing an older open GLEP
  if you're submitting a competing idea.

- Now write your Abstract, Rationale, and other content for your GLEP,
  replacing all of this gobbledygook with your own text.  Be sure to adhere
  to the format guidelines of GLEP 2 [#GLEP2]_, specifically on the
  indentation requirements.

- Update your References section.  You should leave the Copyright section
  as-is, since all new GLEPs are to be licensed under the Creative Commons
  Attribution-ShareAlike License, Version 3.0.

- Send your GLEP submission to the GLEP editors at glep@gentoo.org.


Abstract
========

A short (~200 word) description of the technical issue being addressed.

This is a sample template for creating your own reStructuredText GLEPs.
In conjunction with the content guidelines in GLEP 1 [#GLEP1]_, this should
make it easy for you to conform your own GLEPs to the format outlined below.


Motivation
==========

Provide adequate motivation here.  In this particular case, we need to provide
people with the information necessary to submit GLEPs in the proper form.


Specification
=============

The technical specification should describe the specific areas of Gentoo Linux
that would be touched by this GLEP.  If new functionality is being introduced,
what packages will that functionality affect?  If new policy, who will be
affected?


Rationale
=========

The rationale fleshes out the specification by describing what motivated
the design and why particular design decisions were made.  It should describe
alternate designs that were considered and related work, e.g. how the feature
is supported in other distributions.

The rationale should provide evidence of consensus within the community and
discuss important objections or concerns raised during discussion.


Backwards Compatibility
=======================

Not a problem for this GLEP.  This section should be included *even* when it
is only to state that there are no backwards compatibility issues.


Reference Implementation
========================

The reference implementation must be completed before any GLEP is given
status \"Final\", but it need not be completed before the GLEP is accepted.
It is better to finish the specification and rationale first and reach
consensus on it before writing code or significantly modifying ebuilds.


References
==========

.. [#GLEP1] GLEP 1, GLEP Purpose and Guidelines, Goodyear,
   (http://glep.gentoo.org/glep-0001.html)

.. [#GLEP2] GLEP 2, Sample ReStructuredText GLEP Template, Goodyear,
   (http://glep.gentoo.org/glep-0002.html)


Copyright
=========

This work is licensed under the Creative Commons
Attribution-ShareAlike 3.0 Unported License.  To view a copy of this
license, visit http://creativecommons.org/licenses/by-sa/3.0/.
")

(define-key glep-mode-map
  "\C-c\C-n" 'glep-mode-insert-skeleton)

(easy-menu-define glep-mode-menu glep-mode-map
  "Menu for glep-mode."
  `("GLEP"
    ["Insert skeleton" glep-mode-insert-skeleton]))

;;;###autoload
(add-to-list 'auto-mode-alist '("/glep.*\\.rst\\'" . glep-mode))

(provide 'glep-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; glep-mode.el ends here
