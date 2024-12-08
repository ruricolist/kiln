;;; Build phase 0: compile. Needs to be a separate phase to avoid
;;; serializing foreign pointers.
#+sbcl (require :asdf)
(asdf:upgrade-asdf)
(let ((quicklisp (uiop:getenvp "KILN_QUICKLISP")))
  (when quicklisp
    (load quicklisp)))
(setf uiop/image::*lisp-interaction* nil)
(format *error-output* "Lisp version: ~a ~a~%"
        (lisp-implementation-type)
        (lisp-implementation-version))
(let ((asdf-version (asdf:asdf-version))
      (required-version "3.3.3.2"))
  (format *error-output* "ASDF version: ~a~%" asdf-version)
  (when (uiop:version< asdf-version required-version)
    (format *error-output*
            "Error: Kiln requires ASDF >= ~a~%"
            required-version)
    (finish-output *error-output*)
    (uiop:quit 1)))
(defparameter *target-system* (uiop:getenv "KILN_TARGET_SYSTEM"))
(assert (stringp *target-system*))
(assert (not (= 0 (length *target-system*))))
(if (find-package :ql)
    (progn
      (format *error-output* "Found Quicklisp~%")
      (uiop:symbol-call :ql :register-local-projects)
      (multiple-value-call #'uiop:symbol-call
        :ql :quickload *target-system*
        (if (uiop:getenvp "KILN_DEBUG") (values)
            (values :silent t))))
  (progn
    (format *error-output* "Quicklisp not found~%")
    (asdf:load-system *target-system*)))
(kiln/image:load-all-script-systems)
(finish-output *error-output*)
(uiop:quit)
