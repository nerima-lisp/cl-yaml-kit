(in-package #:yaml-kit)

(defconstant +default-max-input-length+ 104857600)
(defconstant +default-max-depth+ 1000)
(defconstant +default-max-scalar-length+ 16777216)
(defconstant +default-max-nodes+ 1000000)
(defconstant +default-max-alias-expansions+ 100000)

(defun %parser-input-string (input)
  (typecase input
    (string (coerce input 'simple-string))
    (stream (with-output-to-string (out) (loop for c = (read-char input nil nil) while c do (write-char c out))))
    ((vector (unsigned-byte 8))
     (handler-case
         (coerce (cl-codec-kit:octets-to-string input :encoding :auto :errorp t)
                 'simple-string)
       (cl-codec-kit:decode-error (condition)
         (error 'yaml-parse-error :context "invalid YAML input"
                :message (princ-to-string condition)))))
    (otherwise (error 'yaml-parse-error :context "invalid YAML input"))))

(defun map-events (handler input &key (max-input-length +default-max-input-length+)
                                      (max-depth +default-max-depth+)
                                      (max-scalar-length +default-max-scalar-length+))
  (let ((text (%parser-input-string input)))
    (when (> (length text) max-input-length)
      (error 'yaml-resource-limit-error :limit-name "input length"
             :limit max-input-length :actual (length text)))
    (let ((parser (make-parser% :scanner (make-scanner text)
                                :state #'yaml-parser-parse-stream-start
                                :states nil :handler handler :directives nil
                                :version nil :depth 0 :max-depth max-depth
                                :max-scalar-length max-scalar-length)))
      (loop for state = (parser-state parser)
            while state
            do (setf (parser-state parser) (funcall state parser)))))
  nil)

(defun parse-events (input &rest keys)
  (let ((events nil)) (apply #'map-events (lambda (event) (push event events)) input keys) (nreverse events)))
