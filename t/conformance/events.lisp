(in-package #:cl-yaml-kit/test)

(defun conformance-unescape (text)
  (with-output-to-string (out)
    (loop for i from 0 below (length text)
          for character = (char text i)
          do (if (and (char= character #\\) (< (1+ i) (length text)))
                 (progn
                   (incf i)
                   (write-char (case (char text i)
                                 (#\n #\Newline) (#\t #\Tab) (#\r #\Return)
                                 (#\b #\Backspace) (#\\ #\\)
                                 (otherwise (char text i))) out))
                 (write-char character out)))))

(defun conformance-event-fields (text)
  (loop with position = 0
        with fields = nil
        do (loop while (and (< position (length text))
                            (member (char text position) '(#\Space #\Tab)))
                 do (incf position))
           (if (or (= position (length text))
                   (member (char text position) '(#\: #\' #\" #\| #\>)))
               (return (values (nreverse fields) position))
               (let ((end (or (position #\Space text :start position)
                              (position #\Tab text :start position)
                              (length text))))
                 (push (subseq text position end) fields)
                 (setf position end)))))

(defun conformance-field-value (field prefix suffix)
  (when (and field (>= (length field) (+ (length prefix) (length suffix)))
             (string= prefix field :end2 (length prefix))
             (string= suffix field :start2 (- (length field) (length suffix))))
    (subseq field (length prefix) (- (length field) (length suffix)))))

(defun conformance-anchor (field)
  (conformance-field-value field "&" ""))

(defun conformance-tag (field)
  (or (conformance-field-value field "<" ">")
      (conformance-field-value field "!" "")))

(defun conformance-event-anchor (fields)
  (loop for field in fields
        for anchor = (conformance-anchor field)
        when anchor do (return anchor)))

(defun conformance-event-tag (fields)
  (loop for field in fields
        for tag = (conformance-tag field)
        when tag do (return tag)))

(defun conformance-event-flow-p (fields)
  (member "[]" fields :test #'string=))

(defun conformance-event-line (line)
  (let* ((text (string-right-trim '(#\Return) line))
         (kind (if (>= (length text) 4) (subseq text 0 4) text)))
    (cond
      ((string= kind "+STR") (yaml-kit:make-stream-start-event))
      ((string= kind "-STR") (yaml-kit:make-stream-end-event))
      ((string= kind "+DOC")
       (yaml-kit:make-document-start-event
        :explicit-p (string= (string-trim '(#\Space #\Tab)
                                          (subseq text 4))
                             "---")))
      ((string= kind "-DOC")
       (yaml-kit:make-document-end-event
        :explicit-p (string= (string-trim '(#\Space #\Tab)
                                          (subseq text 4))
                             "...")))
      ((member kind '("+SEQ" "+MAP") :test #'string=)
       (let* ((fields (conformance-event-fields (subseq text 4)))
              (sequence-p (string= kind "+SEQ")))
         (if sequence-p
             (yaml-kit:make-sequence-start-event
              :style (if (conformance-event-flow-p fields) :flow :block)
              :anchor (conformance-event-anchor fields)
              :tag (conformance-event-tag fields))
             (yaml-kit:make-mapping-start-event
              :style (if (conformance-event-flow-p fields) :flow :block)
              :anchor (conformance-event-anchor fields)
              :tag (conformance-event-tag fields)))))
      ((member kind '("-SEQ" "-MAP") :test #'string=)
       (if (string= kind "-SEQ")
           (yaml-kit:make-sequence-end-event)
           (yaml-kit:make-mapping-end-event)))
      ((string= kind "=ALI")
       (yaml-kit:make-alias-event
        :anchor (string-left-trim '(#\Space #\*) (subseq text 4))))
      ((string= kind "=VAL")
       (let* ((rest (string-left-trim '(#\Space #\Tab) (subseq text 4)))
              (fields nil)
              (field-end 0))
         (multiple-value-setq (fields field-end)
           (conformance-event-fields rest))
         (let* ((tail (string-left-trim '(#\Space #\Tab)
                                         (subseq rest field-end)))
              (style (and (plusp (length tail)) (char tail 0))))
           (yaml-kit:make-scalar-event
            :anchor (conformance-event-anchor fields)
            :tag (conformance-event-tag fields)
            :style (case style
                     (#\' :single-quoted) (#\" :double-quoted)
                     (#\| :literal) (#\> :folded) (otherwise :plain))
            :value (conformance-unescape (if (plusp (length tail))
                                             (subseq tail 1) ""))))))
      (t (error "Unknown conformance event line: ~S" line)))))

(defun conformance-events (text)
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
          collect (conformance-event-line line)))

(defun conformance-event-signature (event)
  (cond
    ((yaml-kit:stream-start-event-p event) '(:stream-start))
    ((yaml-kit:stream-end-event-p event) '(:stream-end))
    ((yaml-kit:document-start-event-p event)
     (list :document-start (yaml-kit:document-start-event-explicit-p event)))
    ((yaml-kit:document-end-event-p event)
     (list :document-end (yaml-kit:document-end-event-explicit-p event)))
    ((yaml-kit:sequence-start-event-p event)
     (list :sequence-start (eq (yaml-kit:sequence-start-event-style event) :flow)
           (yaml-kit:sequence-start-event-anchor event)
           (yaml-kit:sequence-start-event-tag event)))
    ((yaml-kit:mapping-start-event-p event)
     (list :mapping-start (eq (yaml-kit:mapping-start-event-style event) :flow)
           (yaml-kit:mapping-start-event-anchor event)
           (yaml-kit:mapping-start-event-tag event)))
    ((yaml-kit:sequence-end-event-p event) '(:sequence-end))
    ((yaml-kit:mapping-end-event-p event) '(:mapping-end))
    ((yaml-kit:alias-event-p event)
     (list :alias (yaml-kit:alias-event-anchor event)))
    ((yaml-kit:scalar-event-p event)
     (list :scalar (yaml-kit:scalar-event-anchor event)
           (yaml-kit:scalar-event-tag event)
           (case (yaml-kit:scalar-event-style event)
             (:plain #\:) (:single-quoted #\') (:double-quoted #\")
             (:literal #\|) (:folded #\>) (otherwise #\:))
           (yaml-kit:scalar-event-value event)))))

(defun conformance-event-signatures (text)
  (mapcar #'conformance-event-signature (conformance-events text)))

(describe "conformance event construction"
  (it "parses event prefixes and preserves scalar payloads"
    (dolist (case '(("=VAL &a <tag:yaml.org,2002:str> :value"
                     (:scalar "a" "tag:yaml.org,2002:str" #\: "value"))
                    ("=VAL ' " (:scalar nil nil #\' " "))
                    ("=VAL :" (:scalar nil nil #\: ""))
                    ("+MAP {} &node <tag:yaml.org,2002:map>"
                     (:mapping-start nil "node" "tag:yaml.org,2002:map"))))
      (destructuring-bind (line expected) case
        (expect (conformance-event-signature (conformance-event-line line))
                :to-equal expected)))))
