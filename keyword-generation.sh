#!/bin/bash
# Copyright 2011-2012 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
# Ulrich Müller <ulm@gentoo.org>
#
# Generate a raw list for app-emacs/gentoo-syntax

TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"
ECLASSDIR="$(portageq portdir)/eclass"
ECLASSES=$(cd ${ECLASSDIR}; ls *.eclass)
# Obsolete eclasses or ones which contain no functions
OBSOLETE="bash-completion gems git gnome.org gnustep-2 java-mvn-src \
    kde4-meta-pkg leechcraft mythtv obs-download qt4 ruby"

has() {
    local needle=$1 item
    shift
    for item in "$@"; do
        [[ ${item} = ${needle} ]] && return 0
    done
    return 1
}

echo Output in ${TMPFILE}
echo Manual parsing of mercurial.eclass needed! Do not forget!

for eclass in ${ECLASSES}; do
    has ${eclass%.eclass} ${OBSOLETE} && continue
    grep -q "^# @DEAD$" "${ECLASSDIR}/${eclass}" && continue
    echo '(defvar ebuild-mode-keywords-'${eclass//.eclass/} >>${TMPFILE}
    echo -n \'\(\($(LC_ALL=C grep '^[A-Za-z0-9._-]*()' "${ECLASSDIR}/${eclass}"|sed -e 's:\(^.*\)().*:"\1":g')\)>>${TMPFILE}
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
