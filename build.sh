#!/bin/sh

set -eux

LISP=${LISP:-sbcl}
: "${KILN_HEAP_SIZE:=32768}"
: "${KILN_STACK_SIZE:=}"
export KILN_TARGET_SYSTEM="${KILN_TARGET_SYSTEM:-"kiln/build"}"

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

if [ "$LISP" = "sbcl" ]; then
    LISP_CMD=sbcl_run
elif [ "$LISP" = "ccl" ]; then
    LISP_CMD=ccl_run
fi

# NB The trailing : is needed not to shadow the default source
# registry.
CL_SOURCE_REGISTRY="$(pwd):"
export CL_SOURCE_REGISTRY

# Load once, then dump to avoid serializing foreign pointers.
${LISP_CMD} --load bootstrap/build0.lisp
${LISP_CMD} --load bootstrap/build1.lisp
chmod +x "$tmpfile"
test -n "$("$tmpfile" version)"
mv -f "$tmpfile" "$real_target_file"
if test -z "${NO_PRINT_VERSION:-}"; then
    # real_target_file may be a relative path.
    PATH=$(pwd):$PATH "${real_target_file}" version
fi
