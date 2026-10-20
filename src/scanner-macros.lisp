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

(defmacro %fetch-scanned-token (scanner token-form)
  (let ((scanner-var (gensym "SCANNER-")))
    `(let ((,scanner-var ,scanner))
       (save-simple-key ,scanner-var)
       (setf (scanner-simple-key-allowed ,scanner-var) nil)
       (enqueue-token ,scanner-var ,token-form))))

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
         ((or cl-codec-kit:invalid-leading-byte
              cl-codec-kit:invalid-continuation-byte
              cl-codec-kit:overlong-sequence
              cl-codec-kit:surrogate-code-point
              cl-codec-kit:code-point-too-large
              cl-codec-kit:truncated-sequence) ()
          (sc-error ,scanner-var "while parsing a tag" ,start-var
                    "invalid UTF-8 URI escape"))))))
