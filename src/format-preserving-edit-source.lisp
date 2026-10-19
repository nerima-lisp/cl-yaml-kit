(in-package #:yaml-kit)

(defun %format-edit-path-error (path document operation message)
  (error 'yaml-format-edit-path-error :path path :document document :operation operation :diagnostic (cl-parser-kit:make-diagnostic :message message)))

(defun %format-edit-structure-error (path message)
  (error 'yaml-format-edit-structure-error :path path :diagnostic (cl-parser-kit:make-diagnostic :message message)))

(defun %format-edit-anchor-error (path anchor message)
  (error 'yaml-format-edit-anchor-error :path path :anchor anchor :diagnostic (cl-parser-kit:make-diagnostic :message message)))

(defun %format-edit-octet-p (object)
  (typep object '(vector (unsigned-byte 8))))

(defun %format-edit-prefix-p (octets prefix)
  (and (>= (length octets) (length prefix))
       (loop for i below (length prefix)
             always (= (aref octets i) (aref prefix i)))))

(defun %format-edit-source-encoding (octets)
  (cond
    ((%format-edit-prefix-p octets #(239 187 191)) (values :utf-8 3))
    ((%format-edit-prefix-p octets #(254 255)) (values :utf-16be 2))
    ((%format-edit-prefix-p octets #(255 254 0 0)) (values :utf-32le 4))
    ((%format-edit-prefix-p octets #(255 254)) (values :utf-16le 2))
    ((%format-edit-prefix-p octets #(0 0 254 255)) (values :utf-32be 4))
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
    (t (values :utf-8 0))))

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
         (error 'yaml-parse-error :context "invalid source encoding" :diagnostic (cl-parser-kit:make-diagnostic :message (format nil "invalid source encoding: ~A" (princ-to-string condition)))))))
    (t
     (%format-edit-path-error path document operation
                         "source must be a string or octet vector"))))

(defun %format-edit-line-start (text position)
  (loop with index = (min position (length text))
        while (plusp index)
        do (if (member (char text (1- index)) '(#\Return #\Newline))
               (return index)
               (decf index))
        finally (return 0)))

(defun %format-edit-line-end (text position)
  (loop for index from (min position (length text)) below (length text)
        when (member (char text index) '(#\Return #\Newline))
          do (return index)
        finally (return (length text))))

(defun %format-edit-after-newline (text position)
  (cond
    ((>= position (length text)) position)
    ((and (char= (char text position) #\Return)
          (< (1+ position) (length text))
          (char= (char text (1+ position)) #\Newline)) (+ position 2))
    ((member (char text position) '(#\Return #\Newline)) (1+ position))
    (t position)))

(defun %format-edit-newline (text)
  (let ((position (position-if (lambda (character)
                                (member character '(#\Return #\Newline)))
                              text)))
    (if (null position)
        (string #\Newline)
        (if (and (char= (char text position) #\Return)
                 (< (1+ position) (length text))
                 (char= (char text (1+ position)) #\Newline))
            (format nil "~C~C" #\Return #\Newline)
            (string (char text position))))))

(defun %format-edit-block-span (text start end)
  (let ((line-start (%format-edit-line-start text start))
        (end-line-start (%format-edit-line-start text end)))
    (values line-start
            (if (= end-line-start end)
                end
                (%format-edit-after-newline text
                                            (%format-edit-line-end text end))))))
