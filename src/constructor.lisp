(in-package #:yaml-kit)

(declaim (inline %collection-tag-compatible-p))

(defun %scalar-kind (node schema)
  (let ((tag (node-tag node)))
    (cond
      ((or (null tag) (and tag (string= tag "?")))
       (if (eq (node-style node) :plain)
           (if (member schema '(:core :json))
                (let ((resolved-tag (resolve-tag node :schema schema)))
                 (or (%tag-kind resolved-tag) :str))
               :str)
           :str))
      ((string= tag "!") :str)
      ((%tag-kind tag) (%tag-kind tag))
      (t :str))))

(defun %parse-number (text kind &optional span)
  (declare (optimize (speed 3) (safety 1)))
  (handler-case
      (case kind
        (:int
         (let* ((sign-position (if (and (> (length text) 0)
                                        (member (char text 0) '(#\+ #\-)))
                                   1 0))
                (sign (if (and (plusp sign-position)
                               (char= (char text 0) #\-)) -1 1)))
           (cond
             ((and (<= (+ sign-position 2) (length text))
                   (char-equal (char text sign-position) #\0)
                   (char-equal (char text (1+ sign-position)) #\o))
              (* sign (parse-integer text :radix 8 :start (+ sign-position 2))))
             ((and (<= (+ sign-position 2) (length text))
                   (char-equal (char text sign-position) #\0)
                   (char-equal (char text (1+ sign-position)) #\x))
              (* sign (parse-integer text :radix 16 :start (+ sign-position 2))))
             (t (parse-integer text)))))
        (:float (%parse-yaml-float text span)))
    (error () (signal-yaml-compose-error :diagnostic (cl-parser-kit:make-diagnostic :message "invalid scalar value" :span span) :cause "invalid scalar value"))))

(defun %construct-scalar (node schema)
  (let* ((text (scalar-node-value node))
         (kind (%scalar-kind node schema))
         (tag (node-tag node)))
    (case kind
      (:invalid-collection
       (signal-yaml-compose-error :diagnostic (cl-parser-kit:make-diagnostic :message "collection tag on scalar" :span (node-start-span node)) :cause "collection tag on scalar"))
      (:null (unless (member text +yaml-core-null-spellings+ :test #'string=)
                (signal-yaml-compose-error :diagnostic (cl-parser-kit:make-diagnostic :message "invalid null scalar" :span (node-start-span node)) :cause "invalid null scalar"))
             +yaml-null+)
      (:bool (cond ((member text +yaml-core-true-spellings+ :test #'string=) t)
                   ((member text +yaml-core-false-spellings+ :test #'string=)
                    +yaml-false+)
                   (t (signal-yaml-compose-error :diagnostic (cl-parser-kit:make-diagnostic :message "invalid boolean scalar" :span (node-start-span node)) :cause "invalid boolean scalar"))))
      (:int (%parse-number text kind (node-start-span node)))
      (:float
       (let ((number (%parse-number text kind (node-start-span node))))
         (if (or (%float-nan-p number) (%float-infinity-p number)
                 (not (or (null tag) (string= tag "?"))))
             number
             (handler-case
                 (multiple-value-bind (integer remainder) (truncate number)
                   (if (zerop remainder) integer number))
               (arithmetic-error () number)))))
      (t text))))

(defun %collection-tag-compatible-p (node expected)
  (let ((tag (node-tag node)))
    (or (null tag)
        (member tag '("!" "?") :test #'string=)
        (string= (%canonical-tag tag) expected)
        (and (not (%tag-info tag)) (not (%collection-tag-p tag))))))

(defun %construct-scalar-with-context (node schema)
  (handler-case
      (%construct-scalar node schema)
    (yaml-compose-error (condition)
      (error condition))
    (error (condition)
      (signal-yaml-compose-error
       :diagnostic
       (cl-parser-kit:make-diagnostic
        :message (princ-to-string condition)
        :span (node-start-span node))
       :cause "invalid scalar value"))))

(defun %validate-collection-tag (node expected message)
  (unless (%collection-tag-compatible-p node expected)
    (signal-yaml-compose-error
     :diagnostic
     (cl-parser-kit:make-diagnostic
      :message message
      :span (node-start-span node))
     :cause message)))

(defun %construct-node (node schema mapping-type sequence-type
                        duplicate-key-policy)
  (declare (optimize (speed 3) (safety 1)))
  (let ((memo (make-hash-table :test #'eq)))
    (labels ((walk (object)
               (cond
                 ((scalar-node-p object)
                  (%construct-scalar-with-context object schema))
                 ((sequence-node-p object)
                  (%validate-collection-tag
                   object +yaml-schema-sequence-tag+
                   "incompatible sequence tag")
                  (%construct-sequence object sequence-type memo #'walk))
                 ((mapping-node-p object)
                  (%validate-collection-tag
                   object +yaml-schema-mapping-tag+
                   "incompatible mapping tag")
                  (%construct-mapping object mapping-type duplicate-key-policy
                                      memo #'walk))
                 (t (signal-yaml-compose-error :cause "unsupported node type")))))
      (walk node))))

(defun construct (node &key (schema :core) (mapping-type :hash-table)
                              (sequence-type :vector)
                              (duplicate-key-policy :error))
  "Construct NODE.  Non-scalar hash-table keys are rejected explicitly."
  (declare (optimize (speed 3) (safety 1)))
  (validate-schema schema)
  (unless (member mapping-type '(:hash-table :alist :yaml-mapping))
    (signal-yaml-compose-error :cause "invalid mapping type"))
  (unless (member sequence-type '(:vector :list))
    (signal-yaml-compose-error :cause "invalid sequence type"))
  (unless (member duplicate-key-policy '(:error :first :last))
    (signal-yaml-compose-error :cause "invalid duplicate-key policy"))
  (%construct-node node schema mapping-type sequence-type
                   duplicate-key-policy))

(defun %constructor-construct/cps
    (node k &key (schema :core) (mapping-type :hash-table)
               (sequence-type :vector) (duplicate-key-policy :error))
  (unless (functionp k)
    (error 'type-error :datum k :expected-type 'function))
  (funcall k
           (construct node
                      :schema schema
                      :mapping-type mapping-type
                      :sequence-type sequence-type
                      :duplicate-key-policy duplicate-key-policy)))
