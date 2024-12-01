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
(defun load-system (system)
  (assert (stringp system))
  (assert (not (= 0 (length system))))
  (if (find-package :ql)
      (progn
        (format *error-output* "Found Quicklisp~%")
        (uiop:symbol-call :ql :register-local-projects)
        (multiple-value-call #'uiop:symbol-call
          :ql :quickload system
          (if (uiop:getenvp "KILN_DEBUG") (values)
              (values :silent t))))
      (progn
        (format *error-output* "Quicklisp not found~%")
        (asdf:load-system system))))
(load-system "kiln/build")
(let ((target-system (uiop:getenvp "KILN_TARGET_SYSTEM")))
  (if target-system
      (load-system target-system)
      (progn
        #+sbcl
        (setf sb-ext:*on-package-variance* '(:warn t))
        #+sbcl
        (when (uiop:getenvp "KILN_POIU")
          (handler-case
              (progn
                (sb-ext:unlock-package :sb-sys)
                (let ((key (intern "DEFAULT_INTERRUPT" :sb-sys)))
                  (unless (fboundp key)
                    (export key :sb-sys)
                    (eval
                     `(defun ,key (signal)
                        (sb-sys:enable-interrupt signal :default)))))
                (sb-ext:lock-package :sb-sys)
                (load-system "poiu"))
            (error (e)
              (format *error-output* "Could not load POIU: ~a"
                      e))))
        (kiln/image:load-all-script-systems))))
(finish-output *error-output*)
(uiop:quit)
