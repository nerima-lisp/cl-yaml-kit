;;;; src/schema.lisp
(in-package #:yaml-kit)
(defun validate-schema (schema)
  (unless (member schema +yaml-schema-names+)
    (signal-yaml-compose-error :cause "unknown YAML schema" :message schema))
  schema)

(defun %tag-info (tag)
  (find-if (lambda (row) (or (string= tag (first row))
                             (string= tag (second row))))
           *yaml-tag-table*))

(defun %canonical-tag (tag)
  (let ((row (%tag-info tag))) (if row (second row) tag)))

(defun %tag-kind (tag)
  (let ((row (%tag-info tag))) (and row (third row))))

(defun %collection-tag-p (tag)
  (let ((row (%tag-info tag))) (and row (fourth row))))

(defun %schema-first-characters (rows)
  (let ((bits (make-array 128 :element-type 'bit :initial-element 0)))
    (dolist (row rows (the simple-bit-vector bits))
      (loop for character across (third row)
            do (setf (sbit bits (char-code character)) 1)))))

(defun %schema-row-matches-p (bits value)
  (or (zerop (length value))
      (let ((code (char-code (char value 0))))
        (and (< code (length bits))
             (plusp (sbit bits code))))))

(defun %compile-schema-table (rows)
  (let ((compiled
          (mapcar (lambda (row)
                    (list (concatenate 'string +yaml-tag-prefix+ (first row))
                          (cl-regex-kit:compile-regex (second row))
                          (%schema-first-characters (list row))))
                  rows)))
    (cons (%schema-first-characters rows) compiled)))

;; DEFVAR is intentional. A source reload must not replace compiled regex
;; objects, keeping compilation once per loaded image.
(defvar *compiled-schema-tables*
  (mapcar (lambda (schema)
            (cons (first schema) (%compile-schema-table (rest schema))))
          *schema-spec-tables*))

(defun %schema-table (schema)
  (or (cdr (assoc schema *compiled-schema-tables*))
      (error "Unknown YAML schema: ~S" schema)))

(defun %schema-resolve (value table)
  (declare (optimize (speed 3) (safety 1))
           (type string value))
  (let ((bits (car table)))
    (when (and (plusp (length value))
               (< (char-code (char value 0)) (length bits))
               (zerop (sbit bits (char-code (char value 0)))))
      (return-from %schema-resolve +yaml-schema-string-tag+))
    (dolist (row (cdr table) +yaml-schema-string-tag+)
      (let ((regexp (second row))
            (row-bits (third row)))
        (when (and (%schema-row-matches-p row-bits value)
                   (cl-regex-kit:full-match-p regexp value))
          (return (first row)))))))

(defun %failsafe-kind (value)
  (declare (optimize (speed 3) (safety 1))
           (type string value))
  (%schema-resolve value (load-time-value (%schema-table :failsafe))))

(defmacro define-schema-resolver (name schema)
  `(defun ,name (value)
     (declare (optimize (speed 3) (safety 1))
              (type string value))
     (%schema-resolve value (load-time-value (%schema-table ,schema)))))

(define-schema-resolver %failsafe-kind :failsafe)
(define-schema-resolver %json-kind :json)
(define-schema-resolver %core-kind :core)

(defun resolve-plain-scalar-tag (string schema)
  "Resolve plain STRING to its YAML tag under SCHEMA.

SCHEMA is :FAILSAFE, :JSON, or :CORE. This package-internal contract is
shared by loaders and dumpers when deciding whether STRING may be emitted as
a plain scalar. Quoted and block scalars must not call this resolver."
  (ecase schema
    (:failsafe (%failsafe-kind string))
    (:json (%json-kind string))
    (:core (%core-kind string))))

(defun %implicit-node-tag (node schema)
  (cond ((scalar-node-p node)
         (resolve-plain-scalar-tag (scalar-node-value node) schema))
        ((sequence-node-p node) +yaml-schema-sequence-tag+)
        (t +yaml-schema-mapping-tag+)))

(defun resolve-tag (node &key (schema :core))
  "Return NODE's explicit tag or its implicit schema tag."
  (let ((tag (node-tag node)))
    (if (and tag (not (member tag '("!" "?") :test #'string=)))
        (%canonical-tag tag)
        (if (and tag (scalar-node-p node) (string= tag "!"))
            +yaml-schema-string-tag+
            (%implicit-node-tag node schema)))))
