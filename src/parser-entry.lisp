(in-package #:yaml-kit)

(defconstant +default-max-input-length+ 104857600)
(defconstant +default-max-depth+ 1000)
(defconstant +default-max-scalar-length+ 16777216)
(defconstant +default-max-nodes+ 1000000)
(defconstant +default-max-alias-expansions+ 100000)

(defun %resource-limit (name limit actual)
  (error 'yaml-resource-limit-error :limit-name name :limit limit :actual actual))

(defun %simple-character-string (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun %parser-stream-string (stream max-input-length)
  (%simple-character-string
   (with-output-to-string (out)
     (loop for character = (read-char stream nil nil)
           while character
           for length from 1
           do (when (> length max-input-length)
                (%resource-limit "input length" max-input-length length))
              (write-char character out)))))

(defun %parser-input-string (input max-input-length)
  (typecase input
    (simple-string (the simple-string input))
    (string (%simple-character-string input))
    (stream (%parser-stream-string input max-input-length))
    ((vector (unsigned-byte 8))
     (when (> (length input) max-input-length)
       (%resource-limit "input bytes" max-input-length (length input)))
     (handler-case
         (%simple-character-string
          (cl-codec-kit:octets-to-string input :encoding :auto :errorp t))
       (cl-codec-kit:decode-error (condition)
         (error 'yaml-parse-error :context "invalid YAML input"
                :message (princ-to-string condition)))))
    (otherwise (error 'yaml-parse-error :context "invalid YAML input"))))

(defun map-events (handler input &key
                           (max-input-length +default-max-input-length+)
                           (max-depth +default-max-depth+)
                           (max-scalar-length +default-max-scalar-length+))
  (let ((text (%parser-input-string input max-input-length)))
    (when (> (length text) max-input-length)
      (error 'yaml-resource-limit-error :limit-name "input length"
             :limit max-input-length :actual (length text)))
    (let ((scanner (make-scanner text)))
      (let ((parser (make-parser% :scanner scanner
                                :state 'yaml-parser-parse-stream-start
                                :states nil :handler handler :directives nil
                                :version nil :depth 0 :max-depth max-depth
                                :max-scalar-length max-scalar-length)))
      (loop for state = (parser-state parser)
            while state
            do (setf (parser-state parser) (funcall state parser))
               (recycle-scanner-token scanner)))))
  nil)

(defun parse-events (input &rest keys)
  (let ((events nil)) (apply #'map-events (lambda (event) (push event events)) input keys) (nreverse events)))
