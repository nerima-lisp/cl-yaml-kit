;;;; cl-yaml-kit benchmark corpus and cl-weave measurements.
(require :asdf)

(let* ((script (or *load-truename* *compile-file-truename*))
       (directory (uiop:pathname-directory-pathname script))
       (root (uiop:pathname-parent-directory-pathname directory))
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
(defconstant +max-benchmark-items+ 100000)
(defconstant +max-benchmark-depth+ 1000)
(defconstant +max-benchmark-scalar-length+ 16000000)
(defconstant +max-benchmark-config-bytes+ 67108600)
(defconstant +max-benchmark-iterations+ 1000)
(defconstant +max-benchmark-samples+ 100)
(defconstant +max-benchmark-warmup+ 1000)
(defstruct corpus-case name input value)

;;; This is the complete corpus declaration. Builders contain generation logic;
;;; changing coverage means changing this table, not the measurement loop.
(defparameter *case-table*
  `(("large-block-mapping" block-mapping-input "BENCH_BLOCK_MAPPING_ITEMS" 512 ,+max-benchmark-items+)
    ("large-block-sequence" block-sequence-input "BENCH_BLOCK_SEQUENCE_ITEMS" 512 ,+max-benchmark-items+)
    ("deep-nesting" deep-input "BENCH_DEEP_DEPTH" 1000 ,+max-benchmark-depth+)
    ("long-plain-scalar" plain-scalar-input "BENCH_LONG_SCALAR_LENGTH" 65536 ,+max-benchmark-scalar-length+)
    ("long-double-quoted-scalar" double-quoted-scalar-input "BENCH_LONG_SCALAR_LENGTH" 65536 ,+max-benchmark-scalar-length+)
    ("long-block-literal" block-literal-input "BENCH_LONG_SCALAR_LENGTH" 65536 ,+max-benchmark-scalar-length+)
    ("flow-collection-heavy" flow-collection-input "BENCH_FLOW_ITEMS" 512 ,+max-benchmark-items+)
    ("anchor-alias-heavy" anchor-alias-input "BENCH_ANCHOR_ITEMS" 128 ,+max-benchmark-items+)
    ("realistic-config-1mb" realistic-config-input "BENCH_CONFIG_BYTES" 1048576 ,+max-benchmark-config-bytes+)))

(defun environment-integer (name default minimum maximum)
  (let ((text (uiop:getenv name)))
    (if (null text)
        default
        (let ((value (parse-integer text :junk-allowed nil)))
          (unless (<= minimum value maximum)
            (error "~A must be between ~D and ~D" name minimum maximum))
          value))))

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

(defun plain-scalar-input (length)
  (with-output-to-string (stream)
    (write-string "text: " stream)
    (loop repeat length do (write-char #\x stream))
    (terpri stream)))

(defun double-quoted-scalar-input (length)
  (with-output-to-string (stream)
    (write-string "text: \"" stream)
    (loop repeat length do (write-char #\x stream))
    (write-string "\"" stream)
    (terpri stream)))

(defun block-literal-input (length)
  (with-output-to-string (stream)
    (write-string "text: |" stream)
    (terpri stream)
    (loop with remaining = length
          while (plusp remaining)
          for line-length = (min 80 remaining)
          do (write-string "  " stream)
             (loop repeat line-length do (write-char #\x stream))
             (terpri stream)
             (decf remaining line-length))))

(defun flow-collection-input (items)
  (with-output-to-string (stream)
    (format stream "items: [")
    (loop for index below items
          for first = t then nil
          do (unless first (write-string ", " stream))
             (format stream "{name: item-~4,'0D, values: [~D, ~D, ~D]}"
                     index index (1+ index) (* 2 index)))
    (write-char #\] stream)
    (terpri stream)))

(defun anchor-alias-input (items)
  (with-output-to-string (stream)
    (format stream "base: &base {name: shared, values: [1, 2, 3]}~%")
    (loop for index below items
          do (format stream "item-~4,'0D: *base~%" index))))

(defun realistic-config-input (target-bytes)
  (with-output-to-string (stream)
    (format stream "version: 1~%service:~%  name: yaml-kit~%  environment: production~%  endpoints:~%")
    (loop with bytes = 0
          for index from 0
          while (< bytes target-bytes)
          for line = (format nil "    - name: endpoint-~D~%      url: /api/~D~%      timeout-ms: 3000~%      retries: 3~%" index index)
          do (write-string line stream)
             (incf bytes (length line)))
    (format stream "features:~%  parsing: true~%  emitting: true~%  profile: stable~%")))

(defun build-corpus-case (name builder env default maximum)
  (let* ((size (environment-integer env default 1 maximum))
         (input (funcall builder size))
         (value (yaml-kit:parse input)))
    (when (string= env "BENCH_CONFIG_BYTES")
      (unless (<= (length input) maximum)
        (error "~A generated ~D bytes, exceeding the maximum ~D"
               name (length input) maximum)))
    (unless (plusp (length input))
      (error "Benchmark corpus produced no input for ~A" name))
    (make-corpus-case :name name :input input :value value)))

(defun make-corpus-cases ()
  (loop for (name builder env default maximum) in *case-table*
        collect (build-corpus-case name builder env default maximum)))

(defun bytes-consed ()
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl 0)

(defun run-operation (operation corpus-case)
  (let ((input (corpus-case-input corpus-case))
        (value (corpus-case-value corpus-case)))
    (ecase operation
      (:reader (lambda () (yaml-kit:parse-events input)))
      (:loader (lambda () (yaml-kit:parse input)))
      (:dumper (lambda () (yaml-kit:emit value))))))

(defun measure-operation (thunk input-bytes iterations warmup samples)
  (let ((gc-start (get-internal-real-time)))
    #+sbcl (sb-ext:gc :full t)
    (let ((consed-before (bytes-consed))
          (result (cl-weave:measure thunk :warmup warmup :samples samples
                                     :iterations iterations)))
      (list :mb-per-second
            (let ((milliseconds (cl-weave:median-ms result)))
              (if (plusp milliseconds)
                  (/ (* input-bytes 1000d0) +mib+ milliseconds)
                  0d0))
            :consed-bytes
            (/ (- (bytes-consed) consed-before)
               (* samples iterations))
            :gc-seconds
            (/ (- (get-internal-real-time) gc-start)
               (float internal-time-units-per-second 1d0))))))

(defun tsv (fields)
  (loop for field in fields
        for first = t then nil
        do (unless first (write-char #\Tab)) (princ field))
  (terpri))

(defun main ()
  (let ((iterations (environment-integer "BENCH_ITERATIONS" 3 1 +max-benchmark-iterations+))
        (warmup (environment-integer "BENCH_WARMUP" 1 0 +max-benchmark-warmup+))
        (samples (environment-integer "BENCH_SAMPLES" 5 1 +max-benchmark-samples+))
        (failures 0))
    (format *error-output* "cl-yaml-kit benchmark: cl-weave; iterations=~D; warmup=~D; samples=~D~%"
            iterations warmup samples)
    (tsv '("case" "stage" "status" "input_bytes" "mib_per_second" "consed_bytes" "gc_seconds"))
    (dolist (corpus-case (make-corpus-cases))
      (dolist (operation '(:reader :loader :dumper))
        (handler-case
            (let* ((input-bytes (length (corpus-case-input corpus-case)))
                   (thunk (run-operation operation corpus-case))
                   (result (measure-operation thunk input-bytes iterations warmup samples)))
              (tsv (list (corpus-case-name corpus-case) (string-downcase operation) "ok"
                         input-bytes (format nil "~,6F" (getf result :mb-per-second))
                         (round (getf result :consed-bytes))
                         (format nil "~,6F" (getf result :gc-seconds)))))
          (error (condition)
            (incf failures)
            (format *error-output* "benchmark ~A/~A unavailable: ~A~%"
                    (corpus-case-name corpus-case) operation condition)
            (tsv (list (corpus-case-name corpus-case) (string-downcase operation)
                       "error" (length (corpus-case-input corpus-case)) "-" "-" "-"))))))
    (when (plusp failures)
      (uiop:quit 1))))

(main)
