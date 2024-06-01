(defpackage :kiln/scripts/rebuild
  (:use :cl :alexandria :serapeum :cmd
        :kiln/path :kiln/flags
        :lisp-invocation)
  (:import-from :clingon)
  (:documentation "Rebuild kiln executable in-place")
  (:export :main))
(in-package :kiln/scripts/rebuild)

(def target-system "kiln/build")
(def target-file "kiln")

(defun rebuild (&key
                  (lisp :sbcl)
                  (quicklisp t)
                  (target-system target-system)
                  (target-file target-file))
  (declare (keyword lisp)
           (boolean quicklisp)
           (string target-system))
  (let ((build0 (asdf:system-relative-pathname "kiln"
                                               "bootstrap/build0.lisp"))
        (build1 (asdf:system-relative-pathname "kiln"
                                               "bootstrap/build1.lisp"))
        (quicklisp-setup
          (when quicklisp
            (if (find-package :ql)
                (asdf:system-relative-pathname "quicklisp" "../setup.lisp")
                (error "Quicklisp requested but not available")))))
    (flet ((load-arglist (file)
             (list* "env"
                    (fmt "KILN_TARGET_SYSTEM=~a" target-system)
                    (fmt "KILN_TARGET_FILE=~a" target-file)
                    (lisp-invocation-arglist
                     :implementation-type lisp
                     :load
                     (if quicklisp-setup
                         (list quicklisp-setup file)
                         file)
                     :eval (quit-form :code 0 :implementation-type lisp))))
           (run-arglist (arglist)
             (when (dbg?)
               (format t "Run: ~a~%" arglist))
             (uiop:run-program arglist
                               :output (and (dbg?) *standard-output*)
                               :error-output (and (dbg?) *error-output*))))
      (flet ((call/timing (fn)
               (let ((start (get-internal-real-time)))
                 (multiple-value-prog1 (funcall fn)
                   (let ((end (get-internal-real-time)))
                     (format *error-output* " (~,2fs)~%"
                             (float
                              (/ (- end start)
                                 internal-time-units-per-second)
                              0d0)))))))
        (macrolet ((with-timing (() &body body)
                     (with-thunk (body)
                       `(call/timing ,body))))
          (format *error-output* "Updating fasls~%")
          (with-timing ()
            (run-arglist (load-arglist build0)))
          (format *error-output* "Saving image~%")
          (with-timing ()
            (run-arglist (load-arglist build1))))))))

(def options
  (list
   (clingon:make-option
    :string
    :description "Lisp implementation"
    :long-name "lisp"
    :initial-value (string-downcase (lisp-implementation-type))
    :env-vars '("LISP")
    :key :lisp)
   (clingon:make-option
    :boolean
    :description "Quicklisp?"
    :long-name "quicklisp"
    :initial-value :true
    :key :quicklisp)
   (clingon:make-option
    :string
    :description "Target system"
    :long-name "target-system"
    :initial-value target-system
    :env-vars '("KILN_TARGET_SYSTEM")
    :key :target-system)
   (clingon:make-option
    :string
    :description "Target file"
    :long-name "target-file"
    :initial-value target-file
    :key :target-file)
   ;; For debugging
   (clingon:make-option
    :flag
    :description "Print version"
    :long-name "no-version"
    :initial-value :false
    :key :no-version)))

(def command
  (clingon:make-command
   :name "kiln-rebuild"
   :description "Rebuild Kiln"
   :options options))

(defun main (args)
  (let ((opts (clingon:parse-command-line command args)))
    (format *error-output* "Rebuilding ~a~%"
            (clingon:getopt opts :target-file))
    (force-output *error-output*)
    (rebuild :lisp
             (find-keyword
              (string-upcase
               (clingon:getopt opts :lisp)))
             :quicklisp
             (clingon:getopt opts :quicklisp)
             :target-system
             (clingon:getopt opts :target-system)
             :target-file
             (clingon:getopt opts :target-file))
    (unless (clingon:getopt opts :no-version)
      (cmd:cmd (list target-file "version")))))
