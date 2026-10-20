(in-package #:yaml-kit)

(%define-format-edit-error
  %format-edit-path-error
  yaml-format-edit-path-error
  (path document operation message)
  (:path path
   :document document
   :operation operation
   :diagnostic (cl-parser-kit:make-diagnostic :message message)))

(%define-format-edit-error
  %format-edit-structure-error
  yaml-format-edit-structure-error
  (path message)
  (:path path
   :diagnostic (cl-parser-kit:make-diagnostic :message message)))

(%define-format-edit-error
  %format-edit-anchor-error
  yaml-format-edit-anchor-error
  (path anchor message)
  (:path path
   :anchor anchor
   :diagnostic (cl-parser-kit:make-diagnostic :message message)))

(defun %format-edit-octet-p (object)
  (typep object '(vector (unsigned-byte 8))))

(defun %format-edit-prefix-p (octets prefix)
  (and (>= (length octets) (length prefix))
       (loop for i below (length prefix)
             always (= (aref octets i) (aref prefix i)))))

(defun %format-edit-source-encoding (octets)
  (let ((bom-entry
          (find-if (lambda (entry)
                     (%format-edit-prefix-p octets (cdr entry)))
                   *format-edit-byte-order-marks*)))
    (if bom-entry
        (values (car bom-entry) (length (cdr bom-entry)))
        (cond
          ((and (>= (length octets) 4)
                (zerop (aref octets 0)) (zerop (aref octets 1))
                (zerop (aref octets 2)) (plusp (aref octets 3)))
           (values :utf-32be 0))
          ((and (>= (length octets) 4)
                (plusp (aref octets 0)) (zerop (aref octets 1))
                (zerop (aref octets 2)) (zerop (aref octets 3)))
           (values :utf-32le 0))
          ((and (>= (length octets) 2)
                (zerop (aref octets 0)) (plusp (aref octets 1)))
           (values :utf-16be 0))
          ((and (>= (length octets) 2)
                (plusp (aref octets 0)) (zerop (aref octets 1)))
           (values :utf-16le 0))
          (t (values :utf-8 0))))))

(defun %format-edit-source-text (source path document operation)
  (cond
   ((stringp source)
    (values (%simple-character-string source) nil))
   ((%format-edit-octet-p source)
    (handler-case
        (multiple-value-bind (encoding bom-length)
            (%format-edit-source-encoding source)
          (let* ((bom (subseq source 0 bom-length))
                 (body (subseq source bom-length))
                 (text (cl-codec-kit:octets-to-string body
                                                      :encoding encoding :errorp t)))
            (values (%simple-character-string text)
                    (lambda (edited)
                      (let ((encoded (cl-codec-kit:string-to-octets
                                      edited :encoding encoding)))
                        (if (plusp bom-length)
                            (concatenate '(vector (unsigned-byte 8)) bom encoded)
                          encoded))))))
      (error (condition)
             (error 'yaml-parse-error
                    :context "invalid source encoding"
                    :diagnostic
                    (cl-parser-kit:make-diagnostic
                     :message
                     (format nil "invalid source encoding: ~A"
                             (princ-to-string condition)))))))
   (t
    (%format-edit-path-error path document operation
                             "source must be a string or octet vector"))))

(defun %format-edit-line-start (text position)
  (loop with index = (min position (length text))
        while (plusp index)
        do (if (yaml-line-break-p (char text (1- index)))
               (return index)
               (decf index))
        finally (return 0)))

(defun %format-edit-line-end (text position)
  (loop for index from (min position (length text)) below (length text)
        when (yaml-line-break-p (char text index))
          do (return index)
        finally (return (length text))))

(defun %format-edit-crlf-at-p (text position)
  (and (<= (+ position (length *format-edit-crlf*)) (length text))
       (string= text *format-edit-crlf*
                :start1 position
                :end1 (+ position (length *format-edit-crlf*)))))

(defun %format-edit-after-newline (text position)
  (cond
    ((>= position (length text)) position)
    ((%format-edit-crlf-at-p text position)
     (+ position (length *format-edit-crlf*)))
    ((yaml-line-break-p (char text position)) (1+ position))
    (t position)))

(defun %format-edit-newline (text)
  (let ((position (position-if #'yaml-line-break-p text)))
    (if (null position)
        (string *format-edit-line-feed-character*)
        (if (%format-edit-crlf-at-p text position)
            *format-edit-crlf*
            (string (char text position))))))

(defun %format-edit-block-span (text start end)
  (let ((line-start (%format-edit-line-start text start))
        (end-line-start (%format-edit-line-start text end)))
    (values line-start
            (if (= end-line-start end)
                end
                (%format-edit-after-newline text
                                            (%format-edit-line-end text end))))))
