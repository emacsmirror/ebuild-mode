#!/bin/bash
# Copyright 2011 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later
# $Id: $

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
#
# Generate a raw list for app-emacs/gentoo-syntax

TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"
ECLASSES=$(cd $(portageq portdir)/eclass/;ls *.eclass)

for filter in git bash-completion gems ruby qt4 php-ext-pecl-r1 \
    php-ext-source-r1 gnome.org # Obsolete eclasses or ones which
				# contain no functions
do
    ECLASSES=${ECLASSES//${filter}.eclass/}
done

echo Output in ${TMPFILE}

for eclass in ${ECLASSES}
do
    echo '(defvar ebuild-mode-keywords-'${eclass//.eclass/} >>${TMPFILE}
    echo -n \'\(\($(grep '^[a-Z_-.0-9]*()' $(portageq portdir)/eclass/${eclass}|sed -e 's:\(^.*\)().*:"\1":g')\)>>${TMPFILE}
    echo >>${TMPFILE}
    echo font-lock-type-face\)\)>>${TMPFILE}
    echo >>${TMPFILE}
done
