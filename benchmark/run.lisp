;;;; Deterministic SBCL benchmark harness for cl-yaml-kit.
(require :asdf)

(let* ((script (or *load-truename* *compile-file-truename*))
       (benchmark-directory (uiop:pathname-directory-pathname script))
       (root (uiop:pathname-parent-directory-pathname benchmark-directory))
       (configured-root (uiop:getenv "CL_YAML_KIT_ROOT")))
  (setf root (if configured-root
                 (uiop:ensure-directory-pathname configured-root)
                 root))
  (asdf:load-asd (merge-pathnames "cl-yaml-kit.asd" root))
  (asdf:load-system "cl-yaml-kit")
  (asdf:load-system "cl-weave"))

(defpackage #:yaml-kit/benchmark (:use #:cl) (:export #:main))
(in-package #:yaml-kit/benchmark)

(defconstant +mib+ (* 1024 1024))
(defstruct corpus-case name input value)
(defstruct benchmark case operation input-bytes thunk validator)

(defun environment-integer (name default minimum)
  (let ((text (uiop:getenv name)))
    (if (null text)
        default
        (let ((value (parse-integer text :junk-allowed nil)))
          (unless (>= value minimum)
            (error "~A must be at least ~D" name minimum))
          value))))

(defun mapping-value (items &key (nested-p nil))
  (yaml-kit:make-yaml-mapping
   (loop for index below items
         collect
         (cons (format nil "key-~4,'0D" index)
               (if nested-p
                   (yaml-kit:make-yaml-mapping
                    (list (cons "name" (format nil "value-~4,'0D" index))
                          (cons "values" #(1 2 3 5))))
                   (format nil "value-~4,'0D" index))))))

(defun block-mapping-input (items)
  (with-output-to-string (stream)
    (loop for index below items
          do (format stream "key-~4,'0D: value-~4,'0D~%" index index))))

(defun block-sequence-input (items)
  (with-output-to-string (stream)
    (loop for index below items
          do (format stream "- value-~4,'0D~%" index))))

(defun deep-input (depth)
  (with-output-to-string (stream)
    (loop for level from 0 below depth
          do (format stream "~V@Tlevel-~D:~%" (* 2 level) level))
    (format stream "~V@Tleaf~%" (* 2 depth))))

(defun deep-value (depth)
  (loop with value = "leaf"
        for level downfrom (1- depth) to 0
        do (setf value (yaml-kit:make-yaml-mapping
                        (list (cons (format nil "level-~D" level) value))))
        finally (return value)))

(defun long-scalar-input (length)
  (with-output-to-string (stream)
    (write-string "text: " stream)
    (dotimes (index length) (declare (ignore index)) (write-char #\x stream))
    (terpri stream)))

(defun anchor-heavy-input (items)
  (with-output-to-string (stream)
    (loop for index below items
          do (format stream "value-~4,'0D: &anchor-~4,'0D scalar-~4,'0D~%"
                     index index index)
             (format stream "alias-~4,'0D: *anchor-~4,'0D~%" index index))))

(defun multi-document-input (documents)
  (with-output-to-string (stream)
    (loop for index below documents
          do (format stream "---~%document: ~D~%value: item-~4,'0D~%...~%"
                     index index))))

(defun make-corpus-cases ()
  (let* ((mapping-items (environment-integer "BENCH_BLOCK_MAPPING_ITEMS" 512 1))
         (sequence-items (environment-integer "BENCH_BLOCK_SEQUENCE_ITEMS" 512 1))
         (depth (environment-integer "BENCH_DEEP_DEPTH" 32 1))
         (scalar-length (environment-integer "BENCH_LONG_SCALAR_LENGTH" 65536 1))
         (anchor-items (environment-integer "BENCH_ANCHOR_ITEMS" 128 1))
         (documents (environment-integer "BENCH_DOCUMENTS" 32 1)))
    (list
     (make-corpus-case :name "large-block-mapping"
                       :input (block-mapping-input mapping-items)
                       :value (mapping-value mapping-items))
     (make-corpus-case :name "large-block-sequence"
                       :input (block-sequence-input sequence-items)
                       :value (loop for index below sequence-items
                                    collect (format nil "value-~4,'0D" index)))
     (make-corpus-case :name "deep-nesting"
                       :input (deep-input depth) :value (deep-value depth))
     (make-corpus-case :name "long-scalar"
                       :input (long-scalar-input scalar-length)
                       :value (make-string scalar-length :initial-element #\x))
     (make-corpus-case :name "anchor-heavy"
                       :input (anchor-heavy-input anchor-items)
                       :value (mapping-value anchor-items))
     (make-corpus-case :name "multi-document"
                       :input (multi-document-input documents)
                       :value (yaml-kit:make-yaml-mapping
                               (list (cons "document" 0)
                                     (cons "value" "item-0000")))))))

(defun make-benchmarks (corpus-case)
  (let ((input (corpus-case-input corpus-case))
        (value (corpus-case-value corpus-case)))
    (unless (plusp (length input))
      (error "Benchmark corpus preflight produced no input for ~A"
             (corpus-case-name corpus-case)))
    (list
     (make-benchmark
      :case corpus-case :operation "parse" :input-bytes (length input)
      :thunk (lambda () (yaml-kit:parse input))
      :validator (lambda () (not (null (yaml-kit:parse input)))))
     (make-benchmark
      :case corpus-case :operation "emit" :input-bytes (length input)
      :thunk (lambda () (yaml-kit:emit value))
      :validator (lambda () (plusp (length (yaml-kit:emit value)))))
     (make-benchmark
      :case corpus-case :operation "map-events" :input-bytes (length input)
      :thunk (lambda ()
               (let ((count 0))
                 (yaml-kit:map-events
                  (lambda (event) (declare (ignore event)) (incf count)) input)
                 count))
      :validator (lambda ()
                   (let ((count 0))
                     (yaml-kit:map-events
                      (lambda (event) (declare (ignore event)) (incf count)) input)
                     (plusp count)))))))

(defun seconds-since (start end)
  (/ (- end start) (float internal-time-units-per-second 1d0)))

(defun measure-sample (benchmark iterations)
  (let ((gc-start (get-internal-real-time)))
    #+sbcl (sb-ext:gc :full t)
    (let* ((gc-seconds (seconds-since gc-start (get-internal-real-time)))
           (consed-before #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0)
           (result (cl-weave:measure (benchmark-thunk benchmark)
                                     :warmup 0 :samples 1
                                     :iterations iterations))
           (elapsed (/ (cl-weave:median-ms result) 1000d0))
           (consed (- #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0 consed-before)))
      (list :mb-per-second (if (plusp elapsed)
                               (/ (* iterations (benchmark-input-bytes benchmark))
                                  +mib+ elapsed)
                               0d0)
            :consed-bytes consed
            :gc-seconds gc-seconds))))

(defun median (values)
  (let* ((sorted (sort (copy-list values) #'<))
         (count (length sorted))
         (middle (floor count 2)))
    (if (oddp count)
        (nth middle sorted)
        (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2d0))))

(defun summarize (samples)
  (flet ((values-of (key) (mapcar (lambda (sample) (getf sample key)) samples)))
    (list :mb-per-second (median (values-of :mb-per-second))
          :consed-bytes (median (values-of :consed-bytes))
          :gc-seconds (median (values-of :gc-seconds)))))

(defun tsv (fields)
  (loop for field in fields
        for first = t then nil
        do (unless first (write-char #\Tab)) (princ field))
  (terpri))

(defun main ()
  (let ((iterations (environment-integer "BENCH_ITERATIONS" 3 1))
        (samples (environment-integer "BENCH_SAMPLES" 5 1))
        (warmup (environment-integer "BENCH_WARMUP" 1 0)))
    (format *error-output*
            "cl-yaml-kit benchmark: corpus=deterministic; iterations=~D; samples=~D; warmup=~D~%"
            iterations samples warmup)
    (tsv '("case" "operation" "status" "input_bytes" "iterations" "samples"
           "median_mb_per_second" "median_consed_bytes" "median_gc_seconds"))
    (dolist (corpus-case (make-corpus-cases))
      (dolist (benchmark (make-benchmarks corpus-case))
        (handler-case
            (progn
              (unless (funcall (benchmark-validator benchmark))
                (error "preflight returned NIL"))
              (dotimes (iteration warmup) (declare (ignore iteration))
                (funcall (benchmark-thunk benchmark)))
              (let ((collected (loop repeat samples collect
                                     (measure-sample benchmark iterations))))
                (let ((summary (summarize collected)))
                  (tsv (list (corpus-case-name corpus-case)
                             (benchmark-operation benchmark) "ok"
                             (benchmark-input-bytes benchmark) iterations samples
                             (format nil "~,6F" (getf summary :mb-per-second))
                             (round (getf summary :consed-bytes))
                             (format nil "~,6F" (getf summary :gc-seconds)))))))
          (error (condition)
            (format *error-output* "benchmark ~A/~A unavailable: ~A~%"
                    (corpus-case-name corpus-case)
                    (benchmark-operation benchmark) condition)
            (tsv (list (corpus-case-name corpus-case)
                       (benchmark-operation benchmark) "error"
                       (benchmark-input-bytes benchmark) "-" "-" "-" "-" "-"))))))))

(main)
