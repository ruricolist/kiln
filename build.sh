#!/bin/sh

set -eux

LISP=${LISP:-sbcl}
export KILN_TARGET_SYSTEM=${KILN_TARGET_SYSTEM:-"kiln/build"}
export KILN_TARGET_FILE=${KILN_TARGET_FILE:-"kiln"}

SBCL_OPTIONS=${SBCL_OPTIONS:-"--noinform --disable-debugger"}
CCL_OPTIONS=${CCL_OPTIONS:-"--batch --quiet"}
LISP_OPTIONS=
if [ "$LISP" = "sbcl" ]; then
    LISP_OPTIONS=${SBCL_OPTIONS}
elif [ "$LISP" = "ccl" ]; then
    LISP_OPTIONS=${CCL_OPTIONS}
fi

# Load once, then dump to avoid serializing foreign pointers.
${LISP} ${LISP_OPTIONS} --load bootstrap/build0.lisp
${LISP} ${LISP_OPTIONS} --load bootstrap/build1.lisp
"./${KILN_TARGET_FILE}" version
