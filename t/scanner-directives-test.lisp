(in-package #:cl-yaml-kit/test)

(defun directive-tokens (text)
  (let ((s (yaml-kit:make-scanner (make-array (length text) :element-type 'character
                                               :initial-contents text))) (out nil))
    (loop for token = (yaml-kit:scanner-next-token s) while token do (push token out))
    (nreverse out)))

(defmacro scanner-directive-cases (&body cases)
  `(progn ,@(mapcar (lambda (case)
                      `(it ,(first case)
                         (let ((actual (remove-if (lambda (token)
                                                    (member (yaml-kit:token-kind token)
                                                            '(:stream-start :stream-end)))
                                                  (directive-tokens ,(second case)))))
                           (expect (mapcar (lambda (token)
                                             (list (yaml-kit:token-kind token)
                                                   (yaml-kit:token-value token)
                                                   (yaml-kit:token-handle token)
                                                   (yaml-kit:token-suffix token)
                                                   (yaml-kit:token-major token)
                                                   (yaml-kit:token-minor token))) actual)
                                   :to-equal ',(third case))))) cases)))

(scanner-directive-cases
  ("YAML version" "%YAML 1.2\n" ((:version-directive nil nil nil 1 2)))
  ("reserved directive 6LVF" "%FOO  bar baz # Should be ignored\n--- \"foo\"\n"
   ((:document-start nil nil nil nil nil) (:scalar "foo" nil nil nil nil nil)))
  ("reserved directive 2LFX" "%FOO  bar baz # Should be ignored\n---\n\"foo\"\n"
   ((:document-start nil nil nil nil nil) (:scalar "foo" nil nil nil nil nil)))
  ("TAG directive" "%TAG !e! tag:example.com,2000:\n"
   ((:tag-directive "tag:example.com,2000:" "!e!" nil nil nil)))
  ("anchors and aliases" "&a *a\n"
   ((:anchor "a" nil nil nil nil) (:alias "a" nil nil nil nil)))
  ("tag forms" "! !!str !e!foo !<tag:yaml.org,2002:str>\n"
   ((:tag nil "" "!" nil nil) (:tag nil "!!" "str" nil nil)
    (:tag nil "!e!" "foo" nil nil) (:tag nil "" "tag:yaml.org,2002:str" nil nil)))
  ("URI escape" "!%21\n" ((:tag nil "!" "!" nil nil))))

(defmacro scanner-directive-errors (&body cases)
  `(progn ,@(mapcar (lambda (case)
                      `(it ,(first case)
                         (let ((raised nil))
                           (handler-case (directive-tokens ,(second case))
                             (yaml-kit:yaml-parse-error () (setf raised t)))
                           (expect raised :to-be-truthy)))) cases)))

(scanner-directive-errors
  ("invalid version" "%YAML 2.0\n")
  ("incomplete version" "%YAML 1.\n")
  ("unterminated verbatim tag" "!<tag:yaml.org,2002:str\n")
  ("empty anchor" "&\n"))
