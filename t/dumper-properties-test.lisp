(in-package #:cl-yaml-kit/test)

(defun dumper-property-generator ()
  (cl-weave:gen-recursive
   (cl-weave:gen-one-of
    (cl-weave:gen-integer :min -20 :max 20)
    (cl-weave:gen-boolean)
    (cl-weave:gen-string :min-length 0 :max-length 12
                         :alphabet "abcXYZ012 あé "))
   (lambda (self)
     (cl-weave:gen-one-of
      (cl-weave:gen-list self :min-length 0 :max-length 4)
      (cl-weave:gen-map
       (lambda (pairs)
         (yaml-kit:make-yaml-mapping
          (remove-duplicates pairs :test #'string= :key #'car)))
       (cl-weave:gen-list
        (cl-weave:gen-tuple
         (cl-weave:gen-string :min-length 1 :max-length 5
                              :alphabet "abcXYZ")
         self)
        :min-length 0 :max-length 3))))
   :max-depth 3))

(defun dumper-roundtrip-canonical-value (value)
  (cond
    ((null value) (list :array nil))
    ((yaml-kit:yaml-null-p value) '(:yaml-null))
    ((yaml-kit:yaml-false-p value) '(:yaml-false))
    ((integerp value) (list :integer value))
    ((stringp value) (list :string value))
    ((hash-table-p value)
     (list :object
           (sort (loop for key being the hash-keys of value using (hash-value item)
                       collect (cons (dumper-roundtrip-canonical-value key)
                                     (dumper-roundtrip-canonical-value item)))
                 #'string< :key #'prin1-to-string)))
    ((yaml-kit:yaml-mapping-p value)
     (list :object
           (sort (mapcar (lambda (entry)
                           (cons (dumper-roundtrip-canonical-value (car entry))
                                 (dumper-roundtrip-canonical-value (cdr entry))))
                         (yaml-kit:yaml-mapping-entries value))
                 #'string< :key #'prin1-to-string)))
    ((vectorp value)
     (list :array (map 'list #'dumper-roundtrip-canonical-value value)))
    ((listp value)
     (list :array (mapcar #'dumper-roundtrip-canonical-value value)))
    ((typep value 'boolean) (list :boolean value))
    (t (list :other value))))

(cl-weave:it-property
 "generated values round-trip through the dumper"
 ((value (dumper-property-generator)))
 (expect (dumper-roundtrip-canonical-value
          (yaml-kit:parse (yaml-kit:emit value)))
         :to-equal
         (dumper-roundtrip-canonical-value value)))

(cl-weave:it-each
 ((#.(format nil "value: |~%  unicode あ~%"))
  (#.(format nil "{a: [1, 2], b: あ}~%")))
 "round-trips values parsed from multiline and flow-style YAML"
 (text)
 (expect (yaml-kit:parse text) :to-round-trip t))

(cl-weave:it
 "round-trips repeated shared structures"
 (let* ((shared (list "shared" 7))
        (value (list shared shared)))
   (expect (dumper-roundtrip-canonical-value
            (yaml-kit:parse (yaml-kit:emit value)))
           :to-equal
           (dumper-roundtrip-canonical-value value))))

(cl-weave:it
 "emits NIL as an empty sequence"
 (expect nil :to-emit-as #.(format nil "[]~%"))
 (expect (dumper-roundtrip-canonical-value
          (yaml-kit:parse (yaml-kit:emit nil)))
         :to-equal
         '(:array nil)))
