#!/bin/sh

set -eu

: "${KILN_DEBUG:=}"
: "${KILN_LISP:=sbcl}"
: "${KILN_HEAP_SIZE:=32768}"
: "${KILN_STACK_SIZE:=}"

if test -n "$KILN_DEBUG"; then
    set -x
fi

export KILN_TARGET_SYSTEM="${KILN_TARGET_SYSTEM:-"kiln/build"}"

# We will rebind KILN_TARGET_FILE to a tmpfile during the build.
real_target_file="${KILN_TARGET_FILE:-"kiln"}"

tmpfile=$(mktemp -p "$(pwd)" -t tmpXXXXXXXXXX)
# ASDF won't overwrite a file that exists.
rm "$tmpfile"
delete_tmpfile() {
    rm -f "$tmpfile"
}
trap delete_tmpfile 0

export KILN_TARGET_FILE="$tmpfile"

sbcl_run() {
    set -e
    sbcl \
        ${KILN_HEAP_SIZE:+--dynamic-space-size "${KILN_HEAP_SIZE}"} \
        ${KILN_STACK_SIZE:+--control-stack-size "${KILN_STACK_SIZE}"} \
        --merge-core-pages \
        --disable-ldb \
        --lose-on-corruption \
        --noinform --disable-debugger \
        --eval '(setf sb-impl::*default-external-format* :utf8)' \
         "$@"
}

ccl_run() {
    set -e
    ccl \
        ${KILN_HEAP_SIZE:+--heap-reserve "${KILN_HEAP_SIZE}"} \
        ${KILN_STACK_SIZE:+--stack-size "${KILN_STACK_SIZE}"} \
        --batch --quiet \
        "$@"
}

if [ "$KILN_LISP" = "sbcl" ]; then
    LISP_CMD=sbcl_run
elif [ "$KILN_LISP" = "ccl" ]; then
    LISP_CMD=ccl_run
fi

# NB The trailing : is needed not to shadow the default source
# registry.
: "${CL_SOURCE_REGISTRY:="$(pwd):"}"
export CL_SOURCE_REGISTRY

# Load once, then dump to avoid serializing foreign pointers.

echo "Updating fasls" >&2
${LISP_CMD} --load bootstrap/build0.lisp

echo "Saving image" >&2
${LISP_CMD} --load bootstrap/build1.lisp

chmod +x "$tmpfile"
test -n "$("$tmpfile" version)"
mv -f "$tmpfile" "$real_target_file"
if test -z "${NO_PRINT_VERSION:-}"; then
    # real_target_file may be a relative path.
    PATH=$(pwd):$PATH "${real_target_file}" version
fi
