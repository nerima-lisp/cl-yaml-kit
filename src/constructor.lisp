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

(defun %parse-number (text kind &optional mark)
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
        (:float (%parse-yaml-float text mark)))
    (error () (signal-yaml-compose-error :mark mark
                                         :cause "invalid scalar value"))))

(defun %construct-scalar (node schema)
  (let* ((text (scalar-node-value node))
         (kind (%scalar-kind node schema))
         (tag (node-tag node)))
    (case kind
      (:invalid-collection
       (signal-yaml-compose-error :mark (node-start-mark node)
                                  :cause "collection tag on scalar"))
      (:null (unless (member text '("" "~" "null" "Null" "NULL")
                              :test #'string=)
                (signal-yaml-compose-error :mark (node-start-mark node)
                                           :cause "invalid null scalar"))
             +yaml-null+)
      (:bool (cond ((member text '("true" "True" "TRUE") :test #'string=) t)
                   ((member text '("false" "False" "FALSE") :test #'string=)
                    +yaml-false+)
                   (t (signal-yaml-compose-error :mark (node-start-mark node)
                                                 :cause "invalid boolean scalar"))))
      (:int (%parse-number text kind (node-start-mark node)))
      (:float
       (let ((number (%parse-number text kind (node-start-mark node))))
         (if (and (or (null tag) (string= tag "?")))
             (handler-case
                 (multiple-value-bind (integer remainder) (truncate number)
                   (if (zerop remainder) integer number))
               (arithmetic-error () number))
             number)))
      (t text))))

(defun %collection-tag-compatible-p (node expected)
  (let ((tag (node-tag node)))
    (or (null tag)
        (member tag '("!" "?") :test #'string=)
        (string= (%canonical-tag tag) expected)
        (and (not (%tag-info tag)) (not (%collection-tag-p tag))))))

(defun construct (node &key (schema :core) (mapping-type :hash-table)
                              (sequence-type :vector)
                              (duplicate-key-policy :error)
                              &allow-other-keys)
  "Construct NODE.  Non-scalar hash-table keys are rejected explicitly."
  (declare (optimize (speed 3) (safety 1)))
  (validate-schema schema)
  (unless (member mapping-type '(:hash-table :alist :yaml-mapping))
    (signal-yaml-compose-error :cause "invalid mapping type"))
  (unless (member sequence-type '(:vector :list))
    (signal-yaml-compose-error :cause "invalid sequence type"))
  (unless (member duplicate-key-policy '(:error :first :last))
    (signal-yaml-compose-error :cause "invalid duplicate-key policy"))
  (let ((memo (make-hash-table :test #'eq)))
    (labels ((walk (object)
               (cond
                 ((scalar-node-p object)
                  (handler-case
                      (%construct-scalar object schema)
                    (yaml-compose-error (condition)
                      (error condition))
                    (error (condition)
                      (signal-yaml-compose-error
                       :mark (node-start-mark object)
                       :cause "invalid scalar value"
                       :message (princ-to-string condition)))))
                 ((sequence-node-p object)
                  (unless (%collection-tag-compatible-p object
                                                         "tag:yaml.org,2002:seq")
                    (signal-yaml-compose-error :mark (node-start-mark object)
                                               :cause "incompatible sequence tag"))
                  (%construct-sequence object sequence-type memo #'walk))
                 ((mapping-node-p object)
                  (unless (%collection-tag-compatible-p object
                                                         "tag:yaml.org,2002:map")
                    (signal-yaml-compose-error :mark (node-start-mark object)
                                               :cause "incompatible mapping tag"))
                  (%construct-mapping object mapping-type duplicate-key-policy
                                       memo #'walk))
                 (t (signal-yaml-compose-error :cause "unsupported node type")))))
      (walk node))))
