(in-package #:cl-yaml-kit/test)

(defun format-edit-octets (text encoding &optional bom)
  (let ((body (cl-codec-kit:string-to-octets text :encoding encoding)))
    (if bom
        (concatenate '(vector (unsigned-byte 8)) bom body)
        body)))

(defun format-edit-lines (newline trailing-p &rest lines)
  (with-output-to-string (out)
    (loop for line in lines
          for first-p = t then nil
          do (unless first-p (write-string newline out))
             (write-string line out)
          finally (when trailing-p (write-string newline out)))))

(describe "format-preserving editing"
  (it "replaces a scalar without changing comments or quoting outside the value"
    (let ((source (format-edit-lines (string #\Newline) t
                                     "plain: old  # keep" "single: 'old' # quote")))
      (expect (yaml-kit:edit-source source '("plain") "new")
              :to-equal (format-edit-lines (string #\Newline) t
                                            "plain: new  # keep" "single: 'old' # quote"))))
  (it "preserves CRLF while replacing a scalar"
    (let ((crlf (format nil "~C~C" #\Return #\Newline)))
      (expect (yaml-kit:edit-source (format-edit-lines crlf t
                                                       "a: old" "# keep" "b: 2")
                                    '("a") "new")
              :to-equal (format-edit-lines crlf t "a: new" "# keep" "b: 2"))))
  (it "replaces a value in a flow collection"
    (expect (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                     "root: {a: old, b: 2} # tail")
                                  '("root" "a") "new")
            :to-equal (format-edit-lines (string #\Newline) t
                                          "root: {a: new, b: 2} # tail")))
  (it "appends a mapping entry in block style"
    (expect (yaml-kit:edit-source (format-edit-lines (string #\Newline) t "a: 1" "b: 2")
                                  '("c") 3)
            :to-equal (format-edit-lines (string #\Newline) nil "a: 1" "b: 2" "c: 3")))
  (it "appends a sequence item in block style"
    (expect (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                     "items:" "  - one" "  - two # keep")
                                  '("items" 2) "three")
            :to-equal (format-edit-lines (string #\Newline) nil
                                          "items:" "  - one" "  - two # keep" "  - three")))
  (it "deletes a mapping entry and its inline comment"
    (expect (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                     "a: 1" "b: 2  # remove" "c: 3")
                                  '("b") nil :operation :delete)
            :to-equal (format-edit-lines (string #\Newline) t "a: 1" "c: 3")))
  (it "deletes a sequence item without changing neighboring lines"
    (expect (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                     "items:" "  - one" "  - two # remove" "  - three")
                                  '("items" 1) nil :operation :delete)
            :to-equal (format-edit-lines (string #\Newline) t "items:" "  - one" "  - three")))
  (it "selects a document by zero-based number"
    (expect (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                     "---" "a: first" "..." "--- # second" "a: old" "...")
                                  '("a") "new" :document 1)
            :to-equal (format-edit-lines (string #\Newline) t
                                          "---" "a: first" "..." "--- # second" "a: new" "...")))
  (it "preserves a UTF-8 BOM in octet input"
    (let* ((bom #(239 187 191))
           (source (format-edit-octets (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                                          t "a: old") :utf-8 bom))
           (edited (yaml-kit:edit-source source '("a") "new")))
      (expect (equalp edited
                      (format-edit-octets
                       (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                          t "a: new") :utf-8 bom))
              :to-be-truthy)))
  (it "preserves a UTF-16LE BOM in octet input"
    (let* ((bom #(255 254))
           (source (format-edit-octets (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                                          t "a: old") :utf-16le bom))
           (edited (yaml-kit:edit-source source '("a") "new")))
      (expect (equalp edited
                      (format-edit-octets
                       (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                          t "a: new") :utf-16le bom))
              :to-be-truthy)))
  (it "rejects edits involving an anchor"
    (expect (handler-case
                (progn (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                                 "base: &base" "  a: old")
                                              '("base" "a") "new")
                       nil)
              (yaml-kit:yaml-format-edit-anchor-error () t))
            :to-be-truthy))
  (it "rejects a path through an alias"
    (expect (handler-case
                (progn (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                                 "base: &base {a: 1}" "ref: *base")
                                              '("ref" "a") 2)
                       nil)
              (yaml-kit:yaml-format-edit-anchor-error () t))
            :to-be-truthy))
  (it "rejects additions to flow collections"
    (expect (handler-case
                (progn (yaml-kit:edit-source (format-edit-lines (string #\Newline) t
                                                                 "root: {a: 1}") '("root" "b") 2)
                       nil)
              (yaml-kit:yaml-format-edit-structure-error () t))
            :to-be-truthy))
  (it "rejects a missing path for deletion"
    (expect (handler-case
                (progn (yaml-kit:edit-source (format-edit-lines (string #\Newline) t "a: 1")
                                             '("b") nil :operation :delete)
                       nil)
              (yaml-kit:yaml-format-edit-path-error () t))
            :to-be-truthy)))
