;;;; src/scanner-macros.lisp
(in-package #:yaml-kit)

(defmacro %fetch-character-token (scanner kind)
  (let ((scanner-var (gensym "SCANNER-"))
        (kind-var (gensym "KIND-"))
        (start-var (gensym "START-")))
    `(let* ((,scanner-var ,scanner)
            (,kind-var ,kind)
            (,start-var (sc-position ,scanner-var)))
       (sc-skip ,scanner-var)
       (enqueue-token
        ,scanner-var
        (make-scanner-token ,scanner-var ,kind-var ,start-var
                            (sc-position ,scanner-var))))))

(defmacro %fetch-scanned-token ((scanner scanner-form) token-form)
  (unless (symbolp scanner)
    (error "Scanner binding must be a symbol: ~S" scanner))
  (let ((scanner-var (gensym "SCANNER-")))
    `(let ((,scanner-var ,scanner-form))
       (let ((,scanner ,scanner-var))
         (save-simple-key ,scanner)
         (setf (scanner-simple-key-allowed ,scanner) nil)
         (enqueue-token ,scanner ,token-form)))))

(defmacro %unroll-indent-and-remove-simple-key (scanner)
  (let ((scanner-var (gensym "SCANNER-")))
    `(let ((,scanner-var ,scanner))
       (unroll-indent ,scanner-var -1)
       (remove-simple-key ,scanner-var))))

(defmacro %scanner-document-indicator-p (scanner character)
  (let ((scanner-var (gensym "SCANNER-"))
        (character-var (gensym "CHARACTER-")))
    `(let* ((,scanner-var ,scanner)
            (,character-var ,character))
       (and (sc-check ,scanner-var ,character-var)
            (sc-check ,scanner-var ,character-var 1)
            (sc-check ,scanner-var ,character-var 2)
            (sc-blankz-p ,scanner-var 3)))))

(defmacro %with-invalid-uri-escape ((scanner start) &body body)
  (let ((scanner-var (gensym "SCANNER-"))
        (start-var (gensym "START-")))
    `(let* ((,scanner-var ,scanner)
            (,start-var ,start))
       (handler-case
           (progn ,@body)
         (cl-codec-kit:invalid-leading-byte ()
           (sc-error ,scanner-var "while parsing a tag" ,start-var
                     "invalid UTF-8 URI escape"))
         (cl-codec-kit:invalid-continuation-byte ()
           (sc-error ,scanner-var "while parsing a tag" ,start-var
                     "invalid UTF-8 URI escape"))
         (cl-codec-kit:overlong-sequence ()
           (sc-error ,scanner-var "while parsing a tag" ,start-var
                     "invalid UTF-8 URI escape"))
         (cl-codec-kit:surrogate-code-point ()
           (sc-error ,scanner-var "while parsing a tag" ,start-var
                     "invalid UTF-8 URI escape"))
         (cl-codec-kit:code-point-too-large ()
           (sc-error ,scanner-var "while parsing a tag" ,start-var
                     "invalid UTF-8 URI escape"))
         (cl-codec-kit:truncated-sequence ()
           (sc-error ,scanner-var "while parsing a tag" ,start-var
                     "invalid UTF-8 URI escape"))))))
