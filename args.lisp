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
    (mappend #'keyword-param-options keyword-params)))

(defun keyword-param-options (keyword-param)
  (labels ((shortp (keyword)
             (length= (string keyword) 1))
           (keyword-name-arg (keyword)
             (if (shortp keyword)
                 (values
                  :short-name
                  (character
                   (string-downcase keyword)))
                 (values
                  :long-name
                  (string-downcase keyword))))
           (subword-word (subword)
             (string-case (string-downcase subword)
               ("dont" "don't")
               (t subword)))
           (keyword-description-arg (keyword var)
             (let ((desc
                     (assure string
                       (if (shortp keyword)
                           (let ((words
                                   (mapcar #'subword-word
                                           (split-sequence #\- (string var)))))
                             (fmt "~:(~a~)~{~^ ~(~a~)~}"
                                  (car words) (cdr words)))
                           ""))))
               (values :description desc)))
           (make-flag-option (var keyword)
             (multiple-value-call #'cli:make-option
               :flag
               ;; TODO If short version, same as --long.
               :key (values (make-keyword var))
               (keyword-description-arg keyword var)
               (keyword-name-arg keyword)))
           (make-string-option (var keyword)
             (multiple-value-call #'cli:make-option
               :string
               :key (values (make-keyword var))
               (keyword-description-arg keyword var)
               (keyword-name-arg keyword))))
    (destructuring-bind ((keyword var) value suppliedp)
        keyword-param
      (declare (ignore value))
      (if suppliedp
          (if (eql var suppliedp)
              (list
               (make-flag-option var keyword))
              (list
               (make-flag-option var (make-keyword suppliedp))
               (make-string-option var keyword)))
          (list
           (make-string-option var keyword))))))

(defmacro with-argument-destructuring
    ((&rest bindings)
     (&rest command-kwargs
      &key argv (name "")
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
and no-flag will be NIL if `--no-flag` was passed.

The description of the command can be provided as a docstring for the
form."
  (let ((description
          (or (and (stringp (car body)) (pop body))
              "")))
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
                   ,@body))))))))
