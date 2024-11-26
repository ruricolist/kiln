;;; Build phase 1: dump the image.
#+sbcl (require :asdf)
(asdf:upgrade-asdf)
(let ((quicklisp (uiop:getenvp "KILN_QUICKLISP")))
  (when quicklisp
    (load quicklisp)))
(setf uiop/image::*lisp-interaction* nil)
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  ;; Force max compression.
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   ;; Core compression isn't really worth the
                   ;; increased startup time, so we use the absolute
                   ;; minimum to get page merging.
                   :compression
                   (if (asdf:version-satisfies
                        (lisp-implementation-version)
                        "2.2.6")
                       -7
                       1)))
(defparameter *target-system* (uiop:getenv "KILN_TARGET_SYSTEM"))
(assert (stringp *target-system*))
(assert (not (= 0 (length *target-system*))))
(setf (asdf/system:component-build-pathname
       (asdf:find-system *target-system*))
      (let ((string (uiop:getenvp "KILN_TARGET_FILE")))
        (if string
            (uiop:parse-unix-namestring string)
            #p"kiln")))
(asdf:make *target-system* :type :program :monolithic t)
(uiop:quit)
