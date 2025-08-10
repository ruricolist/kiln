(defpackage :kiln/args
  (:use :cl :alexandria :serapeum)
  (:export :with-argument-destructuring)
  (:local-nicknames
   (:cli :clingon)))
(in-package :kiln/args)

;;; TODO Use types from declarations to parse.

(defclass param ()
  ((var :type symbol :initarg :var :reader param-var)))

;;; Abstract.
(defclass default-param (param)
  ((default :initarg :default :reader param-default)
   (suppliedp :type symbol :initarg :suppliedp :reader param-suppliedp)))

(defclass required-param (param)
  ())

(defun required-param (name)
  (make 'required-param :var name))

(defclass optional-param (default-param)
  ())

(defun optional-param (name default suppliedp)
  (make 'optional-param :name name :default default :suppliedp suppliedp))

(defclass keyword-param (default-param)
  ((keyword :type :keyword :initarg :keyword :reader param-keyword)))

(defun keyword-param (keyword name default suppliedp)
  (make 'keyword-param
        :keyword keyword
        :var name
        :default default
        :suppliedp suppliedp))

(defclass rest-param (param)
  ())

(defun rest-param (name)
  (make 'rest-param :var name))

(defclass rules ()
  ((required-params
    :type list
    :initarg :required-params
    :reader required-params)
   (optional-params
    :type list
    :initarg :optional-params
    :reader optional-params)
   (rest-param
    :type (or null rest-parame)
    :initarg :rest-param
    :reader rest-param-p)
   (keyword-params
    :type list
    :initarg :keyword-params
    :reader keyword-params)
   (allow-other-keys-p
    :type boolean
    :initarg :allow-other-keys
    :reader allow-other-keys-p)
   (allow-keys-p
    :type boolean
    :initarg :allow-keys
    :reader allow-keys-p)))

(defun keyword-dict (rules)
  (lret ((dict (make-hash-table :test #'equal)))
    (dolist (param (keyword-params rules))
      (let* ((str (string-invert-case (param-keyword param)))
             (prefix
               (case (length str)
                 (0 (error "Empty keyword"))
                 (1 "-")
                 (t "--")))
             (key (string+ prefix str)))
        (setf (@ dict key) param)))))

(defun parse-args (args rules)
  (mvlet* ((rules (ensure-rules rules))
           (required args (parse-required-arguments args rules))
           (optional args (parse-optional-arguments args rules))
           (keywords args (parse-keyword-arguments args rules))
           (rest (parse-rest-argument args rules)))
    (args-hash-table
     (append required optional keywords rest))))

(defun args-hash-table (alist)
  (lret ((alist (reverse alist))
         (ht (make-hash-table)))
    ;; The first (left-most) binding wins.
    (loop for (k . v) in alist do
      (ensure2 (@ ht k) v))))

(defun parse-required-arguments (args rules)
  (let* ((params (required-params rules))
         (len (length params)))
    (if (length< args len)
        (error "Missing required arguments: ~a"
               (drop (length args)
                     (mapcar #'param-var
                             params)))
        (multiple-value-bind (required-args rest)
            (halves args len)
          (values
           (mapcar (op (cons (param-var _) _))
                   params
                   required-args)
           rest)))))

(defun parse-optional-arguments (args rules)
  (if (equal (car args) "--")
      (values nil args)
      (if (no args)
          (values nil args)
          (let ((params (optional-params rules))
                alist)
            (loop for param in params do
              (if (string^= "-" (car args))
                  (return)
                  (push (cons (param-var param)
                              (pop args))
                        alist)))
            (values alist args)))))

(defun parse-keyword-arguments (args rules)
  (if (not (allow-keys-p rules))
      (values nil args)
      (let ((dict (keyword-dict rules))
            alist)
        (nlet parse ((args args))
          (if (no args)
              (values alist args)
              (if (string^= "-" (car args))
                  (let ((param (@ dict (car args))))
                    (if (no param)
                        (if (allow-other-keys-p rules)
                            (parse (cdr args))
                            (econd
                             ((string^= "--" (car args))
                              (error "Unknown long keyword argument: ~a"
                                     (car args)))
                             ((string^= "-" (car args))
                              (error "Unknown short keyword argument: ~a"
                                     (car args)))))
                        (if (string^= "-" (cadr args))
                            (progn
                              (push (cons (param-var param)
                                          nil)
                                    alist)
                              (parse (cdr args)))
                            (progn
                              (push (cons (param-var param)
                                          (cadr args))
                                    alist)
                              (parse (cddr args))))))))))))

(defun parse-rest-argument (args rules)
  (if-let (param (rest-param-p rules))
    (values (cons (param-var param) args)
            nil)
    (if args
        (error "Unbound extra arguments: ~a" args)
        (values nil nil))))

(defun lambda-list-rules (lambda-list)
  (multiple-value-bind
        (required-params
         optional-params
         rest-param-p
         keyword-params
         allow-other-keys-p
         aux-params
         allow-keys-p)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore aux-params))
    (make 'rules
          :required-params
          (mapcar #'required-param required-params)
          :optional-params
          (mapply #'optional-param optional-params)
          :rest-param
          (and rest-param-p
               (rest-param rest-param-p))
          :keyword-params
          (loop for ((keyword var) init suppliedp) in keyword-params
                collect (keyword-param keyword var init suppliedp))
          :allow-other-keys
          allow-other-keys-p
          :allow-keys
          allow-keys-p)))

(defun ensure-rules (x)
  (etypecase x
    (list (lambda-list-rules x))
    (rules x)))

(defun generate-binding-lookups (lambda-list dict-var)
  (multiple-value-bind
        (required-params
         optional-params
         rest-param-p
         keyword-params
         allow-other-keys-p
         aux-params
         allow-keys-p)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys-p allow-keys-p))
    (append
     (mapcar (op `(,_1 (@ ,dict-var ',_1)))
             (append
              required-params
              (mapcar #'car optional-params)
              (ensure-list rest-param-p)
              (mapcar #'cadar keyword-params)))
     aux-params)))

(defmacro with-argument-destructuring ((&rest bindings)
                                       (&key
                                          (argv (uiop:command-line-arguments)))
                                       &body body)
  (with-unique-names (dict)
    `(let ((,dict (parse-args ,argv ',bindings)))
       (let ,(generate-binding-lookups bindings dict)
         ,@body))))

;; (assert (equal '("x" "foo")
;;                (with-argument-destructuring (x &key y)
;;                    (:argv '("x" "-y" "foo") )
;;                  (list x y))))
