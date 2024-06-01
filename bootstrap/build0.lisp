;;; Build phase 0: compile. Needs to be a separate phase to avoid
;;; serializing foreign pointers.
(setf uiop/image::*lisp-interaction* nil)
(defparameter *target-system* (uiop:getenv "KILN_TARGET_SYSTEM"))
(assert (stringp *target-system*))
(assert (not (= 0 (length *target-system*))))
(if (find-package :ql)
    (progn
      (format *error-output* "Found Quicklisp~%")
      (uiop:symbol-call :ql :register-local-projects)
      (uiop:symbol-call :ql :quickload *target-system*))
  (progn
    (format *error-output* "Quicklisp not found~%")
    (asdf:load-system *target-system*)))
(kiln/image:load-all-script-systems)
(uiop:quit)
