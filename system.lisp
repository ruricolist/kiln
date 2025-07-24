(defpackage :kiln/system
  (:use :cl :alexandria :serapeum :kiln/flags)
  (:export :load-system))
(in-package :kiln/system)

(defun get-system-load-function (&key silent)
  (assure function
    (if-let (ql (find-package :ql))
      (let ((fn
              (assure function
                (symbol-function
                 (find-external-symbol (string 'quickload)
                                       ql)))))
        (lambda (system &rest args)
          (multiple-value-call fn system
            (if silent
                (values :silent t)
                (values :verbose t))
            (values-list args))))
      (if (not silent)
          (lambda (system &rest args)
            (multiple-value-call #'asdf:load-system
              system
              (values-list args)
              :verbose t))
          (lambda (system &rest args)
            (let ((*standard-output* (make-broadcast-stream))
                  (*error-output* (make-broadcast-stream)))
              (apply #'asdf:load-system system args)))))))

(defun load-system (system/s &rest args &key (silent (not (dbg?))) tolerant
                    &allow-other-keys)
  (let ((fn (get-system-load-function :silent silent)))
    (dolist (system (ensure-list system/s))
      (block nil
        (handler-bind ((error
                         (lambda (e)
                           (when tolerant
                             (dbg "Skipping ~a because: ~a" system e)
                             (return)))))
          (apply fn
                 system
                 :allow-other-keys t
                 args))))))
