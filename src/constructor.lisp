(in-package #:yaml-kit)

(defun %scalar-kind (node schema)
  (let ((tag (node-tag node)))
    (cond
      ((or (null tag) (and tag (string= tag "?")))
       (if (eq (node-style node) :plain)
           (if (member schema '(:core :json))
               (intern (string-upcase
                        (subseq (resolve-tag node :schema schema)
                                (length +yaml-tag-prefix+))) :keyword)
               :str)
           :str))
      ((member tag '("!!str" "tag:yaml.org,2002:str") :test #'string=) :str)
      ((member tag '("!!int" "tag:yaml.org,2002:int") :test #'string=) :int)
      ((member tag '("!!float" "tag:yaml.org,2002:float") :test #'string=) :float)
      ((member tag '("!!bool" "tag:yaml.org,2002:bool") :test #'string=) :bool)
      ((member tag '("!!null" "tag:yaml.org,2002:null") :test #'string=) :null)
      ((member tag '("!!seq" "!!map" "tag:yaml.org,2002:seq"
                    "tag:yaml.org,2002:map") :test #'string=)
       :invalid-collection)
      (t :str))))

(defun %parse-number (text kind)
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
             ((and (< sign-position (length text))
                   (string-equal (subseq text sign-position (+ sign-position 2)) "0o"))
              (* sign (parse-integer text :radix 8 :start (+ sign-position 2))))
             ((and (< sign-position (length text))
                   (string-equal (subseq text sign-position (+ sign-position 2)) "0x"))
              (* sign (parse-integer text :radix 16 :start (+ sign-position 2))))
             (t (parse-integer text)))))
        (:float
         (cond
           ((member text '(".nan" ".NaN" ".NAN") :test #'string=)
            (sb-kernel:make-double-float #x7ff80000 1))
           ((member text '(".inf" ".Inf" ".INF" "+.inf" "+.Inf" "+.INF")
                    :test #'string=)
            sb-kernel::double-float-positive-infinity)
           ((member text '("-.inf" "-.Inf" "-.INF") :test #'string=)
            sb-kernel::double-float-negative-infinity)
           (t (multiple-value-bind (number end)
                  (read-from-string text)
                (unless (= end (length text))
                  (error 'yaml-compose-error))
                (coerce number 'double-float))))))
    (error () (error 'yaml-compose-error))))

(defun %construct-scalar (node schema)
  (let* ((text (scalar-node-value node))
         (kind (%scalar-kind node schema)))
    (case kind
      (:invalid-collection (error 'yaml-compose-error))
      (:null (unless (member text '("" "~" "null" "Null" "NULL")
                              :test #'string=)
                (error 'yaml-compose-error))
             +yaml-null+)
      (:bool (cond ((member text '("true" "True" "TRUE") :test #'string=) t)
                   ((member text '("false" "False" "FALSE") :test #'string=)
                    +yaml-false+)
                   (t (error 'yaml-compose-error))))
      ((:int :float) (%parse-number text kind))
      (t text))))

(defun construct (node &key (schema :core) (mapping-type :hash-table)
                              (sequence-type :vector)
                              (duplicate-key-policy :error)
                              &allow-other-keys)
  "Construct NODE.  Non-scalar hash-table keys are rejected explicitly."
  (declare (optimize (speed 3) (safety 1)))
  (unless (member mapping-type '(:hash-table :alist :yaml-mapping))
    (error 'yaml-compose-error))
  (unless (member sequence-type '(:vector :list))
    (error 'yaml-compose-error))
  (unless (member duplicate-key-policy '(:error :first :last))
    (error 'yaml-compose-error))
  (let ((memo (make-hash-table :test #'eq)))
    (labels
        ((collection-tag-compatible-p (object expected)
           (let ((tag (node-tag object)))
             (or (null tag)
                 (member tag '("!" "?") :test #'string=)
                 (and (string= expected "tag:yaml.org,2002:seq")
                      (string= tag "!!seq"))
                 (and (string= expected "tag:yaml.org,2002:map")
                      (string= tag "!!map"))
                 (string= tag expected))))
         (walk (object)
           (cond
             ((scalar-node-p object) (%construct-scalar object schema))
             ((sequence-node-p object)
              (unless (collection-tag-compatible-p object "tag:yaml.org,2002:seq")
                (error 'yaml-compose-error))
              (multiple-value-bind (cached presentp) (gethash object memo)
                (if presentp
                    cached
                    (if (eq sequence-type :list)
                        (let ((items (sequence-node-items object)))
                          (if (null items)
                              nil
                              (let ((result (cons nil nil)))
                                (setf (gethash object memo) result)
                                (loop for rest on items
                                      for cell = result then (cdr cell)
                                      do (setf (car cell) (walk (car rest)))
                                      when (cdr rest)
                                        do (setf (cdr cell) (cons nil nil))
                                      finally (setf (cdr cell) nil))
                                result)))
                        (let* ((items (sequence-node-items object))
                               (result (make-array (length items))))
                          (setf (gethash object memo) result)
                          (loop for item in items for i from 0
                                do (setf (aref result i) (walk item)))
                          result)))))
             ((mapping-node-p object)
              (unless (collection-tag-compatible-p object "tag:yaml.org,2002:map")
                (error 'yaml-compose-error))
              (multiple-value-bind (cached presentp) (gethash object memo)
                (if presentp
                    cached
                    (let ((pairs (mapping-node-pairs object)))
                      (case mapping-type
                        (:alist
                         (let ((result (make-list (length pairs)))
                               (tail nil) (last-cell nil))
                           (setf (gethash object memo) result
                                 tail result)
                           (dolist (pair pairs)
                             (let ((key (walk (car pair)))
                                   (value (walk (cdr pair))))
                               (when (and (eq duplicate-key-policy :error)
                                          (assoc key result :test #'equal))
                                 (error 'yaml-compose-error))
                               (unless (and (eq duplicate-key-policy :first)
                                            (assoc key result :test #'equal))
                                 (if (eq duplicate-key-policy :last)
                                     (let ((old (assoc key result :test #'equal)))
                                       (if old (setf (cdr old) value)
                                           (setf (car tail) (cons key value)
                                                 last-cell tail
                                                 tail (cdr tail))))
                                     (setf (car tail) (cons key value)
                                           last-cell tail
                                           tail (cdr tail))))))
                           (when last-cell (setf (cdr last-cell) nil))
                           result))
                        (:yaml-mapping
                         (let* ((result (make-yaml-mapping
                                         (make-list (length pairs))))
                                (entries (yaml-mapping-entries result)))
                           (setf (gethash object memo) result)
                           (loop for pair in pairs
                                 for cell on entries
                                 do (setf (car cell)
                                          (cons (walk (car pair))
                                                (walk (cdr pair)))))
                           result))
                        (:hash-table
                         (let ((table (make-hash-table :test #'equal
                                                       :size (max 1 (length pairs)))))
                           (setf (gethash object memo) table)
                           (dolist (pair pairs table)
                             (let ((key (walk (car pair)))
                                   (value (walk (cdr pair))))
                               (when (or (consp key)
                                         (and (vectorp key) (not (stringp key)))
                                         (yaml-mapping-p key))
                                 (error 'yaml-compose-error))
                               (multiple-value-bind (old presentp) (gethash key table)
                                 (declare (ignore old))
                                 (when (and presentp
                                            (eq duplicate-key-policy :error))
                                   (error 'yaml-compose-error))
                                 (unless (and presentp
                                               (eq duplicate-key-policy :first))
                                   (setf (gethash key table) value))))))))))))
             (t (error 'yaml-compose-error)))))
      (walk node))))
