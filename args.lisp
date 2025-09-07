(defpackage :kiln/args
  (:use :cl :alexandria :serapeum)
  (:export :with-argument-destructuring)
  (:local-nicknames
   (:cli :clingon)))
(in-package :kiln/args)

(defun lambda-list-options (lambda-list)
  (multiple-value-bind
        (required-params
         optional-params
         rest-param-p
         keyword-params
         allow-other-keys-p
         aux-params
         allow-keys-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare
     (ignore
      allow-keys-p
      allow-other-keys-p
      aux-params
      optional-params
      required-params
      rest-param-p))
    (labels ((keyword-name-arg (keyword)
               (if (> (length (string keyword)) 1)
                   (values
                    :long-name
                    (string-downcase keyword))
                   (values
                    :short-name
                    (character
                     (string-downcase keyword)))))
             (make-flag-option (var keyword)
               (multiple-value-call #'cli:make-option
                 :flag
                 ;; TODO If short version, same as --long.
                 :description ""
                 :key (values (make-keyword var))
                 (keyword-name-arg keyword)))
             (make-string-option (var keyword)
               (multiple-value-call #'cli:make-option
                 :string
                 :description ""
                 :key (values (make-keyword var))
                 (keyword-name-arg keyword))))
      (loop for ((keyword var) value suppliedp) in keyword-params
            appending
            (if suppliedp
                (if (eql var suppliedp)
                    (list
                     (make-flag-option var keyword))
                    (list
                     (make-flag-option var (make-keyword suppliedp))
                     (make-string-option var keyword)))
                (list
                 (make-string-option var keyword)))))))

(defmacro with-argument-destructuring
    ((&rest bindings)
     (&rest command-kwargs
      &key
        argv
        (description "")
        (name "")
      &allow-other-keys)
     &body body)
  "Do simple argument destructuring.
Multiple bindings can refer to the same var. The leftmost binding
wins (in terms of defaults). This can be used to add
short (single-char) and long keywords for the same variable.

    # Accepts --long-name or -l.
    (&key long-name ((:l long-name) long-name))

Keyword arguments intended to use as flags should define a supplied-p
variable that is the same as the variable. In this case they do not
consume an argument.

    (&key (flag nil flag) (no-flag t no-flat))

Flags invert their initial value (with `not') when the flag is
present. Thus, in the above, `flag' will be T if `--flag` was passed,
and no-flag will be NIL if `--no-flag` was passed."
  (with-unique-names (options command opts)
    `(let* ((,options
              (load-time-value
               (lambda-list-options ',bindings)))
            (,command
              (cli:make-command
               :options ,options
               :description ,description
               :name ,name
               ,@(remove-from-plist
                  command-kwargs
                  :argv
                  :description
                  :name))))
       ,(multiple-value-bind
              (required-params
               optional-params
               rest-param-p
               keyword-params
               allow-other-keys-p
               aux-params
               allow-keys-p)
            (parse-ordinary-lambda-list bindings)
          (declare (ignore allow-keys-p))
          `(let ((,opts
                   ,(if allow-other-keys-p
                        `(handler-bind ((cli:unknown-option
                                          #'cli:discard-option))
                           (cli:parse-command-line ,command ,argv))
                        `(cli:parse-command-line ,command ,argv))))
             (destructuring-bind (,@required-params
                                  ,@optional-params
                                  ,@(and rest-param-p `(&rest ,rest-param-p))
                                  ,@aux-params)
                 (cli:command-arguments ,opts)
               (let* ,(nub
                       (with-collector (collect*)
                         (loop for ((keyword var) init suppliedp) in keyword-params do
                           (cond ((not suppliedp)
                                  (collect*
                                   `(,var
                                     (or (cli:getopt ,opts ,(make-keyword var))
                                         ,init))))
                                 ((eql var suppliedp)
                                  (collect*
                                   `(,var
                                     (if (cli:getopt ,opts ,(make-keyword var))
                                         (not ,init)))))
                                 (t
                                  (collect*
                                   `(,var (or (cli:getopt ,opts ,(make-keyword var))
                                              init)))
                                  (collect*
                                   `(,suppliedp
                                     (cli:getopt
                                      ,opts
                                      ,(make-keyword suppliedp)))))))))
                 ,@body)))))))
