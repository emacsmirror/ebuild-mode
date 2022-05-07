#!/bin/bash
# Copyright 2011-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2 or later

# Authors:
# Christian Faulhammer <fauli@gentoo.org>
# Ulrich Müller <ulm@gentoo.org>
#
# Generate a raw list for app-emacs/ebuild-mode

REPO=gentoo
# Obsolete eclasses
OBSOLETE=""

# Emacs has a limit of 32 kbyte for the size of regular expressions.
# Unfortunately, this is a hard limit in Emacs' C code, MAX_BUF_SIZE
# in regex.c, which cannot be increased. Therefore, split the list
# into several parts with at most MAX_KEYWORDS keywords; a value of
# 1000 appears to keep the regexp size below the limit.
MAX_KEYWORDS=1000

TMPFILE="$(mktemp ${TMPDIR:-/tmp}/keyword-generation.XXXXXX)"

if [[ -n ${ECLASSDIR} ]]; then
    echo "ECLASSDIR = ${ECLASSDIR}" >&2
    ECLASSES=( "${ECLASSDIR}"/*.eclass )
    # We need to strip to the basename, for correct sort order
    ECLASSES=( "${ECLASSES[@]#"${ECLASSDIR}/"}" )
    ECLASSES=( $(printf "%s\n" "${ECLASSES[@]%.eclass}" | LC_ALL=C sort) )
    ECLASSFILES=( "${ECLASSES[@]/%/.eclass}" )
    ECLASSFILES=( "${ECLASSFILES[@]/#/"${ECLASSDIR}/"}" )
else
    echo "No ECLASSDIR specified - using portageq eclass_path" >&2
    ECLASSES=( $(portageq available_eclasses / ${REPO} | LC_ALL=C sort) )
    ECLASSFILES=( $(portageq eclass_path / ${REPO} "${ECLASSES[@]}") )
fi

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

prefix="(("
suffix=")
font-lock-type-face)"

echo "(defvar ebuild-mode-keywords-eclass" >"${TMPFILE}"
echo "'(${prefix}" >>"${TMPFILE}"

count=0
for (( i = 0; i < ${#ECLASSES[@]}; i++ )); do
    eclass=${ECLASSES[i]}
    file=${ECLASSFILES[i]}
    echo -n "  ${eclass} ... " >&2
    grep -q "^# @DEAD$" "${file}" && { echo "skip (dead)" >&2; continue; }
    has ${eclass} ${OBSOLETE} && { echo "skip (obsolete)" >&2; continue; }

    # Get list of functions defined in eclass
    fn_all=$(env -i bash -c ". ${file}; declare -F" 2>/dev/null \
        | sed 's/.*[[:space:]]//')

    # Parse eclass documentation for internal functions
    fn_internal=$(sed -n '/^# @FUNCTION:/{h;:x;n;/^# @INTERNAL/{g;
        s/^# @[^:]*:[[:space:]]*//;p};/^# @/bx}' "${file}")

    functions=(
        $(echo "${fn_all}" | grep -v '^_' | grep -Fvx "${fn_internal}")
    )
    len=${#functions[@]}
    if (( len == 0 )); then
        echo "warning (no functions)" >&2
        continue
    fi

    {
        (( count += len ))
        if (( count > MAX_KEYWORDS )); then
            count=${len}
            echo "${suffix}"
            echo "${prefix}"
        fi
        echo ";; ${eclass}"
        printf ' "%s"' "${functions[@]}"
        echo
    } >>"${TMPFILE}"
    echo "ok" >&2
done

echo "${suffix}))" >>"${TMPFILE}"

emacs -q --no-site-file --batch \
    --visit "${TMPFILE}" \
    --eval "(emacs-lisp-mode)" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(let ((fill-column 78)
                  (fill-indent-according-to-mode t)
                  (paragraph-start \"^.\"))
              (fill-region (point-min) (point-max)))" \
    --eval "(save-buffer)" --kill || exit 1

sed -i -e "/@@KEYWORDS-BEGIN@@/,/@@KEYWORDS-END@@/{//!d}
/@@KEYWORDS-BEGIN@@/r${TMPFILE}" ebuild-mode-keywords.el || exit 1

rm -f "${TMPFILE}"
exit
