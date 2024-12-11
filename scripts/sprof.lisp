(defpackage :kiln/scripts/sprof
  (:use :cl :alexandria :serapeum)
  (:local-nicknames
   (:bt :bordeaux-threads)
   (:cli :clingon))
  (:import-from :kiln/utils
   :invoke-argv)
  #+sbcl (:import-from :sb-sprof)
  (:documentation "Invoke Kiln command with profiling"))
(in-package :kiln/scripts/sprof)

(def options
  (list
   (cli:make-option
    :flag
    :long-name "all-threads"
    :short-name #\A
    :initial-value :true
    :key :all-threads
    :description "Profile all threads")
   (cli:make-option
    :flag
    :long-name "loop"
    :initial-value :false
    :key :loop
    :description "Profile all threads")
   (cli:make-option
    :choice
    :long-name "report"
    :initial-value "flat"
    :items '("flat" "graph")
    :key :report
    :description "Report style")
   (cli:make-option
    :choice
    :long-name "mode"
    :initial-value "cpu"
    :items '("cpu" "alloc" "mode")
    :key :mode
    :description "Profiling mode")
   (cli:make-option
    :integer
    :long-name "max-samples"
    :initial-value sb-sprof:*max-samples*
    :key :max-samples
    :description "Max samples")
   ;; TODO Float class.
   (cli:make-option
    :string
    :long-name "sample-interval"
    :initial-value (princ-to-string sb-sprof:*sample-interval*)
    :key :sample-interval
    :description "Sample interval")
   (cli:make-option
    :choice
    :long-name "sort-by"
    :initial-value "samples"
    :items '("samples" "cumulative-samples")
    :key :sort-by
    :description "Method for sorting the flat report")
   (cli:make-option
    :choice
    :long-name "sort-order"
    :initial-value "descending"
    :items '("descending" "ascending")
    :key :sort-order
    :description "Order for sorting the flat report")))

(def command
  (cli:make-command
   :name "kiln-sprof"
   :options options))

(defun main (args)
  (let* ((opts (cli:parse-command-line command args))
         (args (cli:command-arguments opts))
         (report (make-keyword (string-upcase (cli:getopt opts :report))))
         (threads (if (cli:getopt opts :all-threads)
                      (list (bt:current-thread))
                      :all))
         (mode (make-keyword (string-upcase (cli:getopt opts :mode))))
         (sort-by (make-keyword (string-upcase (cli:getopt opts :sort-by))))
         (sort-order (make-keyword (string-upcase (cli:getopt opts :sort-order)))))
    (sb-sprof:start-profiling
     :mode mode
     :threads threads
     :max-samples (cli:getopt opts :max-samples)
     :sample-interval (parse-float (cli:getopt opts :sample-interval)))
    (unwind-protect
         (invoke-argv args)
      (progn
        (sb-sprof:stop-profiling)
        (sb-sprof:report
         :type report
         :sort-by sort-by
         :sort-order sort-order)))))
