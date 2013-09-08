#!/bin/bash
# Copyright 2011-2013 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
# Ulrich Müller <ulm@gentoo.org>
#
# Generate a raw list for app-emacs/ebuild-mode

TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"
ECLASSDIR="$(portageq portdir)/eclass"
ECLASSES=$(cd ${ECLASSDIR}; ls *.eclass | sed 's/\.eclass$//' | LC_ALL=C sort)
# Obsolete eclasses
OBSOLETE="bash-completion gems leechcraft ruby x-modular"

has() {
    local needle=$1 item
    shift
    for item in "$@"; do
        [[ ${item} = ${needle} ]] && return 0
    done
    return 1
}

echo "Output in ${TMPFILE}"

for eclass in ${ECLASSES}; do
    has ${eclass} ${OBSOLETE} && continue
    file="${ECLASSDIR}/${eclass}.eclass"
    grep -q "^# @DEAD$" "${file}" && continue

    functions=$(env -i bash -c ". ${file}; declare -F" 2>/dev/null \
        | sed 's/.*[[:space:]]//;/^_/d;s/.*/"&"/')
    [[ -z ${functions} ]] && continue

    {
        echo "(defvar ebuild-mode-keywords-${eclass%.eclass}"
        echo "  '(("${functions}")"
        echo "    font-lock-type-face))"
        echo
    } >>"${TMPFILE}"
done

emacs -q --batch \
    --visit "${TMPFILE}" \
    --eval "(emacs-lisp-mode)" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(let ((fill-column 78)
                  (fill-indent-according-to-mode t)
                  (paragraph-start \"^.\"))
              (fill-region (point-min) (point-max)))" \
    --eval "(save-buffer)" --kill
