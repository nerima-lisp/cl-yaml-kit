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
