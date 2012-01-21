#!/bin/bash
# Copyright 2011-2012 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later
# $Id: $

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
#
# Generate a raw list for app-emacs/gentoo-syntax

TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"
ECLASSES=$(cd $(portageq portdir)/eclass/;ls *.eclass)

# Obsolete eclasses or ones which contain no functions
for filter in git bash-completion gems ruby qt4 php-ext-pecl-r1 \
    php-ext-source-r1 gnome.org gnustep-2 java-mvn-src kde4-meta-pkg \
    leechcraft mythtv obs-download tetex tetex-3
do
    ECLASSES=${ECLASSES//${filter}.eclass/}
done

echo Output in ${TMPFILE}
echo Manual parsing of mercurial.eclass needed! Do not forget!

for eclass in ${ECLASSES}
do
    echo '(defvar ebuild-mode-keywords-'${eclass//.eclass/} >>${TMPFILE}
    echo -n \'\(\($(LC_ALL=C grep '^[A-Za-z0-9._-]*()' $(portageq portdir)/eclass/${eclass}|sed -e 's:\(^.*\)().*:"\1":g')\)>>${TMPFILE}
    echo >>${TMPFILE}
    echo font-lock-type-face\)\)>>${TMPFILE}
    echo >>${TMPFILE}
done

emacs -q --batch \
    --visit ${TMPFILE} \
    --eval "(emacs-lisp-mode)" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(let ((fill-column 78)
                  (fill-indent-according-to-mode t)
                  (paragraph-start \"^.\"))
              (fill-region (point-min) (point-max)))" \
    --eval "(save-buffer)" --kill
