(in-package #:cl-yaml-kit/test)

(describe "loader scalar construction"
  (it "constructs a plain .NaN scalar as a float"
    (let ((construct-scalar (symbol-function 'yaml-kit::%construct-scalar))
          (node (yaml-kit:make-scalar-node :value ".NaN" :style :plain)))
      (expect (floatp (funcall construct-scalar node :core))
              :to-be-truthy)))

  (it "parses explicitly signed decimal and prefixed integers"
    (let ((parse-number (symbol-function 'yaml-kit::%parse-number)))
      (expect (funcall parse-number "-17" :int) :to-equal -17)
      (expect (funcall parse-number "-0o17" :int) :to-equal -15)
      (expect (funcall parse-number "+0x10" :int) :to-equal 16)))

  (it "rejects a base prefix without digits"
    (expect (lambda () (funcall (symbol-function 'yaml-kit::%parse-number)
                                "0o" :int))
            :to-signal-loader-error
            'yaml-kit:yaml-compose-error))

  (cl-weave:it-each
      ((octal "0o17" 15)
       (hexadecimal "0x10" 16))
    "parses unsigned base-prefixed number ~S"
    (name text expected)
    (declare (ignore name))
    (expect (funcall (symbol-function 'yaml-kit::%parse-number) text :int)
            :to-equal
            expected))

  (it "distinguishes implicit scalar kinds from quoted and non-specific strings"
    (let ((scalar-kind (symbol-function 'yaml-kit::%scalar-kind)))
      (expect (funcall scalar-kind
                       (yaml-kit:make-scalar-node :value "true" :style :plain)
                       :core)
              :to-equal :bool)
      (expect (funcall scalar-kind
                       (yaml-kit:make-scalar-node :value "true"
                                                  :style :double-quoted)
                       :core)
              :to-equal :str)
      (expect (funcall scalar-kind
                       (yaml-kit:make-scalar-node :value "true" :tag "!"
                                                  :style :plain)
                       :core)
              :to-equal :str)))

  (it "preserves explicit and non-specific integral float behavior"
    (let ((construct-scalar (symbol-function 'yaml-kit::%construct-scalar)))
      (let ((explicit (funcall construct-scalar
                               (yaml-kit:make-scalar-node :value "1.0"
                                                          :tag "!!float"
                                                          :style :plain)
                               :core))
            (implicit (funcall construct-scalar
                               (yaml-kit:make-scalar-node :value "1.0"
                                                          :tag "?"
                                                          :style :plain)
                               :core))
            (fraction (funcall construct-scalar
                               (yaml-kit:make-scalar-node :value "1.5"
                                                          :tag "?"
                                                          :style :plain)
                               :core)))
        (expect (floatp explicit) :to-be-truthy)
        (expect implicit :to-equal 1)
        (expect (floatp fraction) :to-be-truthy))))

  (it "keeps an explicitly non-specific NaN scalar numeric"
    (let ((construct-scalar (symbol-function 'yaml-kit::%construct-scalar))
          (node (yaml-kit:make-scalar-node :value ".NaN" :tag "?"
                                           :style :plain)))
      (expect (floatp (funcall construct-scalar node :core))
              :to-be-truthy))))
