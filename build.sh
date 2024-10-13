#!/bin/sh

set -eux

LISP=${LISP:-sbcl}
export KILN_TARGET_SYSTEM=${KILN_TARGET_SYSTEM:-"kiln/build"}
export KILN_TARGET_FILE=${KILN_TARGET_FILE:-"kiln"}

sbcl_run() {
    sbcl --noinform --disable-debugger "$@"
}

ccl_run() {
    ccl --batch --quiet
}

if [ "$LISP" = "sbcl" ]; then
    LISP_CMD=sbcl_run
elif [ "$LISP" = "ccl" ]; then
    LISP_CMD=ccl_run
fi

# NB The trailing : is needed not to shadow the default source
# registry.
export CL_SOURCE_REGISTRY="$(pwd):"

# Load once, then dump to avoid serializing foreign pointers.
${LISP_CMD} --load bootstrap/build0.lisp
${LISP_CMD} --load bootstrap/build1.lisp
"./${KILN_TARGET_FILE}" version
