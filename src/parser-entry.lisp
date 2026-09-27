(in-package #:yaml-kit)

(defconstant +default-max-input-length+ 104857600)
(defconstant +default-max-depth+ 1000)
(defconstant +default-max-scalar-length+ 16777216)
(defconstant +default-max-nodes+ 1000000)
(defconstant +default-max-alias-expansions+ 100000)

(defun %parser-octet-encoding (input)
  (let ((n (length input)))
    (cond ((and (>= n 4) (= (aref input 0) 0) (= (aref input 1) 0)
                (= (aref input 2) #xfe) (= (aref input 3) #xff))
           (values :utf-32be 4))
          ((and (>= n 4) (= (aref input 0) #xff) (= (aref input 1) #xfe)
                (= (aref input 2) 0) (= (aref input 3) 0))
           (values :utf-32le 4))
          ((and (>= n 2) (= (aref input 0) #xfe) (= (aref input 1) #xff))
           (values :utf-16be 2))
          ((and (>= n 2) (= (aref input 0) #xff) (= (aref input 1) #xfe))
           (values :utf-16le 2))
          ((and (>= n 3) (= (aref input 0) #xef) (= (aref input 1) #xbb)
                (= (aref input 2) #xbf))
           (values :utf-8 3))
          (t (values :utf-8 0)))))

(defun %parser-input-string (input)
  (typecase input
    (string (coerce input 'simple-string))
    (stream (with-output-to-string (out) (loop for c = (read-char input nil nil) while c do (write-char c out))))
    ((vector (unsigned-byte 8))
     (multiple-value-bind (encoding start) (%parser-octet-encoding input)
       (coerce (cl-codec-kit:octets-to-string input :start start :encoding encoding :errorp t) 'simple-string)))
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
