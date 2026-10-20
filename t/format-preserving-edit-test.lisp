(in-package #:cl-yaml-kit/test)

(defun format-edit-octets (text encoding &optional bom)
  "Encode TEXT using ENCODING, optionally prefixed by BOM."
  (let ((body (cl-codec-kit:string-to-octets text :encoding encoding)))
    (if bom
        (concatenate '(vector (unsigned-byte 8)) bom body)
        body)))

(defun format-edit-lines (newline trailing-p &rest lines)
  "Join LINES with NEWLINE and optionally end with a newline."
  (with-output-to-string (out)
    (loop for line in lines
          for first-p = t then nil
          do (unless first-p (write-string newline out))
             (write-string line out)
          finally (when trailing-p (write-string newline out)))))

(defun format-edit-lf (&rest lines)
  "Build an LF-terminated YAML source from LINES."
  (apply #'format-edit-lines (string #\Newline) t lines))

(defun format-edit-value-at (source document path)
  "Return the value at PATH in document DOCUMENT of SOURCE."
  (let ((value (nth document (yaml-kit:parse-all source))))
    (dolist (component path value)
      (setf value
            (cond
              ((hash-table-p value) (gethash component value))
              ((vectorp value) (aref value component))
              ((listp value) (nth component value))
              (t (error "cannot descend through ~S" value)))))))

(defun format-edit-present-at (source document path)
  "Return whether PATH is present in document DOCUMENT of SOURCE."
  (let ((value (nth document (yaml-kit:parse-all source))))
    (dolist (component path t)
      (cond
        ((hash-table-p value)
         (multiple-value-bind (next presentp) (gethash component value)
           (unless presentp (return-from format-edit-present-at nil))
           (setf value next)))
        ((vectorp value)
         (unless (and (integerp component) (< -1 component (length value)))
           (return-from format-edit-present-at nil))
         (setf value (aref value component)))
        ((listp value)
         (unless (and (integerp component) (< -1 component (length value)))
           (return-from format-edit-present-at nil))
         (setf value (nth component value)))
        (t (return-from format-edit-present-at nil))))))

(defun format-edit-condition (thunk)
  "Return the YAML kit condition raised by THUNK, or NIL when it returns normally."
  (handler-case
      (progn (funcall thunk) nil)
    (yaml-kit:yaml-kit-error (condition)
      condition)))

(defun format-edit-condition-type (thunk)
  "Return the type of the YAML kit condition raised by THUNK."
  (let ((condition (format-edit-condition thunk)))
    (when condition (type-of condition))))

(defun format-edit-expect (source path value expected-source
                           &key (document 0) (operation :set) unchanged
                             (unchanged-document document) (check-deleted t))
  "Assert edited source and semantic invariants for one format edit."
  (let ((edited (yaml-kit:edit-source source path value
                                      :document document :operation operation)))
    (expect edited :to-equal expected-source)
    (if (and (eq operation :delete) check-deleted)
        (expect (format-edit-present-at edited document path) :to-be-falsy)
        (unless (eq operation :delete)
          (expect (format-edit-value-at edited document path) :to-equal value)))
    (expect (loop for sibling-path in unchanged by #'cddr
                  collect (format-edit-value-at edited unchanged-document sibling-path))
            :to-equal
            (loop for sibling-value in unchanged
                  for index from 0
                  when (oddp index) collect sibling-value))))

(describe "format-preserving editing"
  (it "replaces a scalar without changing comments or quoting outside the value"
    (let ((source (format-edit-lf "plain: old  # keep" "single: 'old' # quote")))
      (format-edit-expect
       source '("plain") "new"
       (format-edit-lf "plain: new  # keep" "single: 'old' # quote")
       :unchanged (list '("single") "old"))))
  (it "preserves CRLF while replacing a scalar"
    (let ((crlf (format nil "~C~C" #\Return #\Newline)))
      (format-edit-expect
       (format-edit-lines crlf t "a: old" "# keep" "b: 2")
       '("a") "new"
       (format-edit-lines crlf t "a: new" "# keep" "b: 2")
       :unchanged (list '("b") 2))))
  (it "replaces a flow mapping value and preserves adjacent formatting"
    (format-edit-expect
     (format-edit-lf "root: {a: old, b: 2} # tail")
     '("root" "a") "x, y"
     (format-edit-lf "root: {a: 'x, y', b: 2} # tail")
     :unchanged (list '("root" "b") 2)))
  (it "quotes a flow sequence value containing a closing bracket"
    (format-edit-expect
     (format-edit-lf "items: [a, b]")
     '("items" 0) "x]"
     (format-edit-lf "items: ['x]', b]")
     :unchanged (list '("items" 1) "b")))
  (it "appends a mapping entry in block style and keeps the final newline"
    (format-edit-expect
     (format-edit-lf "a: 1" "b: 2")
     '("c") 3
     (format-edit-lf "a: 1" "b: 2" "c: 3")
     :unchanged (list '("a") 1 '("b") 2)))
  (cl-weave:it-each
      ((value-line-break "key" #.(format nil "line1~%line2"))
       (value-document-start "key" "---")
       (value-document-end "key" "...")
       (value-comment "key" "# comment")
       (value-sequence-indicator "key" "- x")
       (value-boolean "key" "true")
       (value-number "key" "123")
       (value-empty "key" "")
       (value-null "key" "null")
       (key-line-break #.(format nil "line1~%line2") 2)
       (key-document-start "---" 2)
       (key-document-end "..." 2)
       (key-comment "# comment" 2)
       (key-sequence-indicator "- x" 2)
       (key-boolean "true" 2)
       (key-number "123" 2)
       (key-empty "" 2)
       (key-null "null" 2))
    "quotes YAML-boundary inserted string case ~S"
    (name key value)
    (declare (ignore name))
    (let ((edited (yaml-kit:edit-source (format-edit-lf "root: 1")
                                        (list key) value)))
      (expect (format-edit-value-at edited 0 (list key)) :to-equal value)
      (expect (format-edit-value-at edited 0 '("root")) :to-equal 1)))
  (it "adds a mapping entry to a non-final document"
    (let* ((source (format-edit-lf "---" "a: 1" "..." "---" "b: 2" "..."))
           (edited (yaml-kit:edit-source source '("c") 3 :document 0)))
      (expect (format-edit-value-at edited 0 '("c")) :to-equal 3)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal 1)
      (expect (format-edit-value-at edited 1 '("b")) :to-equal 2)))
  (it "adds a nested mapping entry and preserves siblings"
    (let* ((source (format-edit-lf "a:" "  b: 1" "c: 2"))
           (edited (yaml-kit:edit-source source '("a" "d") 3)))
      (expect (format-edit-value-at edited 0 '("a" "d")) :to-equal 3)
      (expect (format-edit-value-at edited 0 '("a" "b")) :to-equal 1)
      (expect (format-edit-value-at edited 0 '("c")) :to-equal 2)))
  (it "inserts a mapping entry before trailing comment blocks"
    (let* ((source (format-edit-lf "a: 1" "# tail" "# keep"))
           (edited (yaml-kit:edit-source source '("b") 2)))
      (expect edited :to-equal
              (format-edit-lf "a: 1" "b: 2" "# tail" "# keep"))
      (expect (list (format-edit-value-at edited 0 '("a"))
                    (format-edit-value-at edited 0 '("b")))
              :to-equal '(1 2))))
  (it "deletes an empty mapping value and preserves neighboring entries"
    (let* ((source (format-edit-lf "a: 1" "b:" "c: 3"))
           (edited (yaml-kit:edit-source source '("b") nil :operation :delete)))
      (expect edited :to-equal (format-edit-lf "a: 1" "c: 3"))
      (expect (format-edit-present-at edited 0 '("b")) :to-be-falsy)
      (expect (list (format-edit-value-at edited 0 '("a"))
                    (format-edit-value-at edited 0 '("c")))
              :to-equal '(1 3))))
  (it "appends a sequence item in block style and keeps the final newline"
    (format-edit-expect
     (format-edit-lf "items:" "  - one" "  - two # keep")
     '("items" 2) "three"
     (format-edit-lf "items:" "  - one" "  - two # keep" "  - three")
     :unchanged (list '("items" 0) "one" '("items" 1) "two")))
  (it "deletes a mapping entry and its inline comment"
    (format-edit-expect
     (format-edit-lf "a: 1" "b: 2  # remove" "c: 3")
     '("b") nil
     (format-edit-lf "a: 1" "c: 3")
     :operation :delete
     :unchanged (list '("a") 1 '("c") 3)))
  (it "deletes a sequence item without changing neighboring lines"
    (format-edit-expect
     (format-edit-lf "items:" "  - one" "  - two # remove" "  - three")
     '("items" 1) nil
     (format-edit-lf "items:" "  - one" "  - three")
     :operation :delete
     :check-deleted nil
     :unchanged (list '("items" 0) "one" '("items" 1) "three")))
  (it "selects a document by zero-based number"
    (format-edit-expect
     (format-edit-lf "---" "a: first" "..." "--- # second" "a: old" "...")
     '("a") "new"
     (format-edit-lf "---" "a: first" "..." "--- # second" "a: new" "...")
     :document 1
     :unchanged-document 0
     :unchanged (list '("a") "first")))
  (cl-weave:it-each
      ((apostrophe "O'Brien, Jr" "'O''Brien, Jr'")
       (colon-space "key: value" "'key: value'")
       (colon-question "key:?value" "'key:?value'")
       (empty "" "''")
       (safe-colon "name:value" "name:value")
       (closing-bracket "x]y" "'x]y'"))
    "quotes YAML-dangerous flow value case ~S"
    (name value fragment)
    (declare (ignore name))
    (format-edit-expect
     (format-edit-lf "root: [old]") '("root" 0) value
     (format-edit-lf (format nil "root: [~A]" fragment))))
  (it "escapes controls and Unicode line separators in replacement strings"
    (let* ((codes '(0 7 8 9 10 11 12 13 27 34 92 133 8232 8233 1 127))
           (escape-codes '("0" "a" "b" "t" "n" "v" "f" "r" "e" "\""
                           "\\" "N" "L" "P" "x01" "x7F"))
           (value (coerce (mapcar #'code-char codes) 'string))
           (fragment
             (with-output-to-string (out)
               (write-char #\" out)
               (loop for escape in escape-codes
                     do (write-char #\\ out)
                        (write-string escape out))
               (write-char #\" out)))
           (edited (yaml-kit:edit-source (format-edit-lf "value: old")
                                         '("value") value)))
      (expect (length codes) :to-equal (length escape-codes))
      (expect (yaml-kit::%format-edit-double-quoted-fragment value)
              :to-equal fragment)
      (expect (format-edit-value-at edited 0 '("value")) :to-equal value)))
  (cl-weave:it-property
    "round-trips generated printable replacement strings"
    ((value (cl-weave:gen-string :min-length 0 :max-length 16
                                 :alphabet "abcXYZ012 あé")))
    (expect (format-edit-value-at
             (yaml-kit:edit-source (format-edit-lf "value: old") '("value") value)
             0 '("value"))
            :to-equal value)))
