#!/bin/bash
# Copyright 2011-2014 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
# Ulrich Müller <ulm@gentoo.org>
#
# Generate a raw list for app-emacs/ebuild-mode

REPO=gentoo
TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"
ECLASSES=( $(portageq available_eclasses / ${REPO} | LC_ALL=C sort) )
ECLASSFILES=( $(portageq eclass_path / ${REPO} "${ECLASSES[@]}") )
# Obsolete eclasses
OBSOLETE="bash-completion gems leechcraft ruby x-modular"

# Arrays should have equal size
[[ ${#ECLASSES[@]} -eq ${#ECLASSFILES[@]} ]] || exit 1

has() {
    local needle=$1 item
    shift
    for item in "$@"; do
        [[ ${item} = ${needle} ]] && return 0
    done
    return 1
}

echo "Output in ${TMPFILE}"

for (( i = 0; i < ${#ECLASSES[@]}; i++ )); do
    eclass=${ECLASSES[i]}
    has ${eclass} ${OBSOLETE} && continue
    file=${ECLASSFILES[i]}
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
