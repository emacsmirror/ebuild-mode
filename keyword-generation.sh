#!/bin/bash
# Copyright 2011-2016 Gentoo Foundation
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
OBSOLETE="bash-completion leechcraft x-modular"

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

    # Get list of functions defined in eclass
    fn_all=$(env -i bash -c ". ${file}; declare -F" 2>/dev/null \
        | sed 's/.*[[:space:]]//')

    # Parse eclass documentation for internal functions
    fn_internal=$(sed -n '/^# @FUNCTION:/{h;:x;n;/^# @INTERNAL/{g;
        s/^# @[^:]*:[[:space:]]*//;p};/^# @/bx}' "${file}")

    functions=$(echo "${fn_all}" | grep -v '^_' | grep -Fvx "${fn_internal}")
    [[ -z ${functions} ]] && continue

    {
        echo "(defvar ebuild-mode-keywords-${eclass%.eclass}"
        echo "  '(("$(echo "${functions}" | sed 's/.*/"&"/')")"
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
