(in-package #:cl-yaml-kit/test)

(describe "format-preserving edit encoding"
  (it "preserves a UTF-8 BOM in octet input"
    (let* ((bom #(239 187 191))
           (source (format-edit-octets
                    (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                       t "a: old") :utf-8 bom))
           (edited (yaml-kit:edit-source source '("a") "new")))
      (expect (equalp edited
                      (format-edit-octets
                       (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                          t "a: new") :utf-8 bom))
              :to-be-truthy)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal "new")))
  (cl-weave:it-each
      ((utf-8 :utf-8 nil)
       (utf-16be-bom :utf-16be #(254 255))
       (utf-32le-bom :utf-32le #(255 254 0 0))
       (utf-32be-bom :utf-32be #(0 0 254 255))
       (utf-16be :utf-16be nil)
       (utf-16le :utf-16le nil)
       (utf-16le-bom :utf-16le #(255 254)))
    "preserves detected ~S input encoding"
    (name encoding bom)
    (declare (ignore name))
    (let* ((source (format-edit-octets (format-edit-lf "a: old") encoding bom))
           (edited (yaml-kit:edit-source source '("a") "new"))
           (expected (format-edit-octets (format-edit-lf "a: new") encoding bom)))
      (expect (equalp edited expected) :to-be-truthy)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal "new")))
  (cl-weave:it-each
      ((trailing-newline t)
       (no-final-newline nil))
    "preserves final-newline state for ~S CRLF additions"
    (name trailing-p)
    (declare (ignore name))
    (let ((crlf (format nil "~C~C" #\Return #\Newline)))
      (format-edit-expect (format-edit-lines crlf trailing-p "a: 1" "b: 2")
                          '("c") 3
                          (format-edit-lines crlf trailing-p "a: 1" "b: 2" "c: 3"))))
  (it "uses the first newline style for mixed-ending additions"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "a: 1" crlf "b: 2" (string #\Newline)))
           (edited (yaml-kit:edit-source source '("c") 3)))
      (expect edited
              :to-equal
              (concatenate 'string "a: 1" crlf "b: 2" (string #\Newline)
                           "c: 3" crlf))
      (expect (format-edit-value-at edited 0 '("c")) :to-equal 3)))
  (cl-weave:it-each
      ((big-endian :utf-32be)
       (little-endian :utf-32le))
    "detects BOM-less ~S input and preserves its bytes"
    (name encoding)
    (declare (ignore name))
    (let* ((source (format-edit-octets (format-edit-lf "a: old") encoding))
           (edited (yaml-kit:edit-source source '("a") "new"))
           (expected (format-edit-octets (format-edit-lf "a: new") encoding)))
      (expect (equalp edited expected) :to-be-truthy)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal "new"))))
