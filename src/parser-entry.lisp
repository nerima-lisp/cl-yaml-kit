(in-package #:yaml-kit)

(defun %simple-character-string (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun %parser-stream-string (stream max-input-length)
  (%simple-character-string
   (with-output-to-string (out)
     (loop for index below (1+ max-input-length)
           for character = (read-char stream nil nil)
           while character
           do (write-char character out)))))

(defun %parser-input-string (input max-input-length)
  (labels ((check-input-length (length unit)
             (when (> length max-input-length)
               (%signal-yaml-resource-limit
                unit max-input-length length :context "YAML"))))
    (typecase input
      (string
       (let ((text (if (typep input 'simple-string)
                       input
                       (%simple-character-string input))))
         (check-input-length (length text) "input length")
         text))
      (stream
       (let ((text (%parser-stream-string input max-input-length)))
         (check-input-length (length text) "input length")
         text))
      ((vector (unsigned-byte 8))
       (check-input-length (length input) "input bytes")
       (handler-case
           (%simple-character-string
            (cl-codec-kit:octets-to-string input :encoding :auto :errorp t))
         (cl-codec-kit:decode-error (condition)
           (error 'yaml-parse-error
                  :context "invalid YAML input"
                  :diagnostic
                  (cl-parser-kit:make-diagnostic
                   :message (format nil "invalid YAML input: ~A"
                                    (princ-to-string condition)))))))
      (otherwise
       (error 'yaml-parse-error
              :context "invalid YAML input"
              :diagnostic
              (cl-parser-kit:make-diagnostic
               :message "invalid YAML input"))))))

(define-yaml-resource-function map-events (handler input)
    (:group :scanner :keywords ())
  (let* ((text (%parser-input-string input max-input-length))
         (scanner (make-scanner text))
         (parser (make-parser% :scanner scanner
                               :state #'yaml-parser-parse-stream-start
                               :states nil :handler handler :directives nil
                               :version nil :depth 0 :max-depth max-depth
                               :max-scalar-length max-scalar-length)))
    (loop for state = (parser-state parser)
          while state
          do (setf (parser-state parser) (funcall state parser))
             (recycle-scanner-token scanner)))
  nil)

(define-yaml-resource-function %parser-run/cps (handler input k)
    (:group :scanner :keywords ())
  (check-type k function)
  (map-events handler input
              :max-input-length max-input-length
              :max-depth max-depth
              :max-scalar-length max-scalar-length)
  (values (funcall k nil)))

(defun parse-events (input &rest keys)
  (let ((events nil))
    (apply #'map-events
           (lambda (event)
             (push event events))
           input
           keys)
    (nreverse events)))

(define-yaml-resource-function map-events/k
    (handler input k)
    (:group :scanner :keywords ())
  (%parser-run/cps handler input k
                   :max-input-length max-input-length
                   :max-depth max-depth
                   :max-scalar-length max-scalar-length))

(define-yaml-resource-function parse-events/k
    (input k)
    (:group :scanner :keywords ())
  (check-type k function)
  (values
   (funcall k
            (parse-events input
                          :max-input-length max-input-length
                          :max-depth max-depth
                          :max-scalar-length max-scalar-length))))
