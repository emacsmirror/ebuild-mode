#!/bin/bash
# Copyright 2011 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later
# $Id: $

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
#
# Generate a raw list for app-emacs/gentoo-syntax

TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"

for i in *.eclass
do
    echo '(defvar ebuild-mode-keywords-'${i//.eclass/} >>${TMPFILE}
    echo -n \'\(\($(grep '^[a-Z_-]*()' $i|sed -e 's:\(^.*\)().*:"\1":g')\)>>${TMPFILE}
    echo >>${TMPFILE}
    echo font-lock-type-face\)\)>>${TMPFILE}
    echo >>${TMPFILE}
done
