(defpackage :kiln/scripts/grep
  (:use :cl :alexandria :serapeum :cl-ppcre :kiln)
  (:import-from :clingon)
  (:import-from :clawk)
  (:documentation "Grep files with CL-PPCRE")
  (:shadowing-import-from :cl-ppcre :scan))
(in-package :kiln/scripts/grep)

;;; TODO Encodings. Chardet?

(def options
  (list
   (clingon:make-option
    :boolean
    :description "Ignore case"
    :long-name "ignore-case"
    :short-name #\i
    :initial-value :false
    :key :ignore-case)
   (clingon:make-option
    :list
    :description "Pattern"
    :long-name "regexp"
    :short-name #\e
    :key :patterns)
   (clingon:make-option
    :boolean
    :description "Never print filename"
    :long-name "no-filename"
    :short-name #\h
    :initial-value :false
    :key :no-filename)
   (clingon:make-option
    :boolean
    :description "Always print filename"
    :long-name "with-filename"
    :short-name #\H
    :initial-value :false
    :key :with-filename)
   (clingon:make-option
    :boolean
    :description "Fixed strings"
    :long-name "fixed-strings"
    :short-name #\F
    :initial-value :false
    :key :fixed-strings)
   (clingon:make-option
    :boolean
    :description "Smart case"
    :long-name "smart-case"
    :initial-value :false
    :short-name #\S
    :key :smart-case)))

(def command
  (clingon:make-command
   :name "kiln-grep"
   :description "Grep files with CL-PPCRE"
   :options options))

(defun main (args)
  (let* ((opts (clingon:parse-command-line command args))
         (ignore-case (clingon:getopt opts :ignore-case))
         (smart-case (clingon:getopt opts :smart-case))
         (files (clingon:command-arguments opts))
         (multiple-files (rest files))
         (patterns
           (or (clingon:getopt opts :patterns)
               (list (pop files))))
         (fixed-strings? (clingon:getopt opts :fixed-strings))
         (scanners
           (unless fixed-strings?
             (mapcar (lambda (pattern)
                       (create-scanner pattern
                                       :case-insensitive-mode
                                       (or ignore-case
                                           (and smart-case
                                                (notany #'upper-case-p pattern)))))
                     patterns)))
         (print-filenames?
           (or (clingon:getopt opts :with-filename)
               (and multiple-files
                    (not (clingon:getopt opts :no-filename))))))
    (with-boolean (print-filenames? fixed-strings?)
      (dolist (file files)
        ;; TODO print file name?
        (handler-case
            (clawk:for-file-lines (file in line)
              (when (:if fixed-strings?
                         (every (op (search _ line)) patterns)
                         (every (op (scan _ line)) scanners))
                (:when print-filenames?
                  (format t "~/tty:color~/~a~/tty:color~/: "
                          :bold
                          (uiop:native-namestring file)
                          nil))
                (format t "~a~%" line)))
          (error (e)
            (format *error-output* "~a" e)))))))
