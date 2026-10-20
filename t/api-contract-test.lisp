(in-package #:cl-yaml-kit/test)

(defun api-documented-symbols ()
  (let ((text (uiop:read-file-string
               (asdf:system-relative-pathname
                :cl-yaml-kit "docs/src/reference/api.md")))
        (symbols nil)
        (start 0))
    (loop for open = (position #\` text :start start)
          while open
          for close = (position #\` text :start (1+ open))
          while close
          for token = (subseq text (1+ open) close)
          for end = (position-if (lambda (character)
                                  (find character '(#\Space #\Tab #\Comma)))
                                token)
          for name = (subseq token 0 end)
          do (multiple-value-bind (symbol status)
                 (find-symbol (string-upcase name) :yaml-kit)
               (when (and symbol (eq status :external))
                 (pushnew symbol symbols)))
             (setf start (1+ close)))
    symbols))

(defun external-api-symbols ()
  (let ((symbols nil))
    (do-external-symbols (symbol :yaml-kit)
      (push symbol symbols))
    symbols))

(defun symbol-names (symbols)
  (sort (mapcar #'symbol-name symbols) #'string<))

(it "keeps the exported API and reference API in sync"
  (expect (symbol-names (external-api-symbols)) :to-equal
          (symbol-names (api-documented-symbols))))

(it "preserves compose error causes through the public accessor"
  (handler-case
      (yaml-kit:compose-events
       (lambda (handler)
         (funcall handler (yaml-kit:make-scalar-event :value "outside"))))
    (yaml-kit:yaml-compose-error (condition)
      (expect (yaml-kit:yaml-compose-error-cause condition)
              :to-equal "event before stream start"))))

(it "preserves unsupported values and their type in emit causes"
  (let ((value (make-condition 'simple-error)))
    (handler-case
        (yaml-kit:emit value)
        (yaml-kit:yaml-emit-error (condition)
        (let ((cause (yaml-kit:yaml-emit-error-cause condition)))
          (expect (typep (getf cause :value) 'simple-error) :to-be-truthy)
          (expect (getf cause :type) :to-equal (type-of value)))))))

(it "keeps sentinel identity stable and constants non-rebindable"
  (expect (constantp 'yaml-kit:+yaml-null+) :to-be-truthy)
  (expect (constantp 'yaml-kit:+yaml-false+) :to-be-truthy)
  (expect (eq yaml-kit:+yaml-null+ :null) :to-be-falsy)
  (expect (eq yaml-kit:+yaml-false+ :false) :to-be-falsy)
  (expect (eq yaml-kit:+yaml-null+ (yaml-kit:parse "null")) :to-be-truthy)
  (expect (eq yaml-kit:+yaml-false+ (yaml-kit:parse "false")) :to-be-truthy))

(cl-weave:it-each
 ((:comment-spans/k)
  (:map-events/k)
  (:parse-events/k)
  (:compose-events/k)
  (:compose-all-events/k)
  (:compose/k)
  (:compose-all/k)
  (:parse/k)
  (:parse-all/k)
  (:read-yaml/k)
  (:emit-events/k)
  (:emit/k)
  (:write-yaml/k)
  (:edit-source/k))
 "returns only the primary callback value from ~S"
 (entry)
 (labels ((callback (value)
            (declare (ignore value))
            (values :primary :secondary))
          (event-source (handler)
            (dolist (event (yaml-kit:parse-events "value"))
              (funcall handler event))))
   (expect
    (multiple-value-list
     (ecase entry
       (:comment-spans/k
        (yaml-kit:comment-spans/k
         "# comment" (function callback) :max-input-length 20))
       (:map-events/k
        (yaml-kit:map-events/k
         (lambda (event) (declare (ignore event)))
         "value" (function callback)
         :max-input-length 5 :max-depth 10 :max-scalar-length 10))
       (:parse-events/k
        (yaml-kit:parse-events/k
         "value" (function callback)
         :max-input-length 5 :max-depth 10 :max-scalar-length 10))
       (:compose-events/k
        (yaml-kit:compose-events/k
         (function event-source) (function callback)
         :max-depth 10 :max-scalar-length 10
         :max-nodes 10 :max-alias-expansions 10))
       (:compose-all-events/k
        (yaml-kit:compose-all-events/k
         (function event-source) (function callback)
         :max-depth 10 :max-scalar-length 10
         :max-nodes 10 :max-alias-expansions 10))
       (:compose/k
        (yaml-kit:compose/k
         "value" (function callback)
         :max-input-length 5 :max-depth 10))
       (:compose-all/k
        (yaml-kit:compose-all/k
         (format nil "first~%---~%second") (function callback)
         :max-input-length 100 :max-depth 10))
       (:parse/k
        (yaml-kit:parse/k
         "value" (function callback) :schema :core
         :max-input-length 5 :max-depth 10))
       (:parse-all/k
        (yaml-kit:parse-all/k
         (format nil "first~%---~%second") (function callback)
         :schema :core :max-input-length 100 :max-depth 10))
       (:read-yaml/k
        (with-input-from-string (stream "value")
          (yaml-kit:read-yaml/k
           stream (function callback) :schema :core
           :max-input-length 5 :max-depth 10)))
       (:emit-events/k
        (yaml-kit:emit-events/k
         (yaml-kit:parse-events "value") (function callback) :indent 2))
       (:emit/k
        (yaml-kit:emit/k "value" (function callback) :indent 2))
       (:write-yaml/k
        (yaml-kit:write-yaml/k
         "value" (make-string-output-stream)
         (function callback) :indent 2))
       (:edit-source/k
        (yaml-kit:edit-source/k
         (format nil "a: 1~%") (list "a") 2 :k (function callback)))))
    :to-equal (list :primary))))
