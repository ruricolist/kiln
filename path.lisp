(defpackage :kiln/path
  (:use :cl :alexandria :serapeum :kiln/stamp)
  (:import-from :kiln/flags :+kiln-path-systems+)
  (:import-from :uiop
   :file-exists-p :getenv :getenvp :parse-unix-namestring)
  (:import-from :trivia :match)
  (:import-from :trivial-types :pathname-designator)
  (:export
   #:scripts-path
   #:list-all-scripts
   #:script->system
   #:system-scripts
   #:list-all-script-subsystems
   #:list-all-scripts-by-system
   #:*builtins-by-system*
   #:script
   #:script-name
   #:script-system
   #:script-subsystem
   #:script-stamp
   #:script-path
   #:script-package-docs
   #:list-loaded-scripts
   #:script-package
   #:*builtins-by-system-table*
   #:builtins-by-system-table))
(in-package :kiln/path)

(deftype path-list ()
  '(soft-list-of string))

(declaim
 (type path-list
       *internal-scripts-path*
       *local-scripts-path*))

(defvar *internal-scripts-path*
  '("kiln/scripts"))

(defvar *local-scripts-path*
  '("local-scripts"))

(defclass script ()
  ((name :initarg :name :reader script-name :type string)
   (path :initarg :path :reader script-path :type pathname)
   (system :initarg :system :reader script-system :type string)
   (subsystem :initarg :subsystem :reader script-subsystem :type string)
   (package :initarg :package :reader script-package :type string)
   (stamp :initarg :stamp :reader script-stamp :type stamp)
   (package-docs :initarg :package-docs :reader script-package-docs
                 :type (or string null))))

(defmethod print-object ((self script) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (script-name self))))

(-> scripts-path () path-list)
(defun scripts-path ()
  (append *local-scripts-path*
          (when-let (env (getenvp +kiln-path-systems+))
            (split-sequence-if (op (find _  ",;:"))
                               env
                               :remove-empty-subseqs t))
          *internal-scripts-path*))

(-> system-scripts (string) (soft-list-of script))
(defun system-scripts (system-name)
  (when (asdf:find-system system-name nil)
    (let* ((root (asdf:system-relative-pathname system-name ""))
           (root
             (if-let (pos (position #\/ system-name))
               (parse-unix-namestring
                (string+ (uiop:unix-namestring root)
                         (drop pos system-name)
                         "/"))
               root))
           (system-lisp-files
             (directory
              (make-pathname :defaults root
                             :name :wild
                             :type "lisp")))
           (system-lisp-files
             (remove-if (op (string^= "#." (pathname-name _)))
                        system-list-files)))
      (loop for file in system-lisp-files
            for script-name = (pathname-name file)
            for subsystem-name = (string+ system-name "/" script-name)
            when (asdf:find-system system-name nil)
              collect (make 'script
                            :name script-name
                            :path file
                            :stamp (file-stamp file)
                            :system system-name
                            :subsystem subsystem-name
                            :package (string-upcase subsystem-name)
                            :package-docs (get-package-docs file))))))

(-> get-package-docs (pathname-designator)
    (or null string))
(defun get-package-docs (path)
  (when (file-exists-p path)
    (let* ((*read-eval* nil)
           (*print-circle* nil)
           (form
             (with-input-from-file (in path)
               (read in))))
      (match form
        ((list* _ _ props)
         (some (lambda (prop)
                 (match prop
                   ((list :documentation docs)
                    docs)))
               props))))))

(-> list-all-scripts () (values (soft-list-of script) &optional))
(defun list-all-scripts ()
  (mappend #'system-scripts (scripts-path)))

(-> list-all-script-subsystems () (soft-list-of string))
(defun list-all-script-subsystems ()
  (mapcar #'script-subsystem (list-all-scripts)))

(-> list-all-scripts-by-system () (soft-alist-of string list))
(defun list-all-scripts-by-system ()
  (mapcar (op (cons _1 (system-scripts _1)))
          ;; Later shadows former.
          (scripts-path)))

(defvar *builtins-by-system*
  ()
  "Alist from system names to lists of scripts.")

(defvar-unbound *builtins-by-system-table*
  "Hash table from system names to hash tables of names to scripts.")

(defun builtins-by-system-table (alist)
  (alist-hash-table
   (mapcar (lambda (cons)
             (cons (car cons)
                   (set-hash-table (cdr cons)
                                   :test #'equal
                                   :key #'script-name)))
           alist)
   :test #'equal
   :size (length alist)))

(defun list-loaded-scripts ()
  (nub
   (mapcar #'script-name
           (mappend #'cdr *builtins-by-system*))))
