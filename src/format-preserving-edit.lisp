(in-package #:yaml-kit)

(defstruct (format-edit-node (:constructor %make-format-edit-node)
                             (:copier nil))
  kind
  start
  end
  anchor
  style
  value
  children
  pairs)

(defstruct (format-edit-pair (:constructor %make-format-edit-pair)
                             (:copier nil))
  key
  value)

(defun %format-edit-error (type path document operation message)
  (error type :path path :document document :operation operation :message message))

(defun %format-edit-structure-error (path message)
  (error 'yaml-format-edit-structure-error :path path :message message))

(defun %format-edit-anchor-error (path anchor message)
  (error 'yaml-format-edit-anchor-error :path path :anchor anchor :message message))

(defun %format-edit-event-offset (event accessor)
  (let ((mark (funcall accessor event)))
    (unless mark
      (%format-edit-structure-error nil "event has no source mark"))
    (mark-offset mark)))

(defun %format-edit-node-from-events (events)
  (let ((events (coerce events 'vector)))
    (labels ((parse-node (index)
               (let ((event (aref events index)))
                 (cond
                   ((scalar-event-p event)
                    (values (%make-format-edit-node
                             :kind :scalar
                             :start (%format-edit-event-offset event #'event-start-mark)
                             :end (%format-edit-event-offset event #'event-end-mark)
                             :anchor (scalar-event-anchor event)
                             :style (scalar-event-style event)
                             :value (scalar-event-value event))
                            (1+ index)))
                   ((alias-event-p event)
                    (values (%make-format-edit-node
                             :kind :alias
                             :start (%format-edit-event-offset event #'event-start-mark)
                             :end (%format-edit-event-offset event #'event-end-mark)
                             :anchor (alias-event-anchor event))
                            (1+ index)))
                   ((sequence-start-event-p event)
                    (let ((children nil)
                          (next (1+ index)))
                      (loop until (sequence-end-event-p (aref events next))
                            do (multiple-value-bind (child new-next) (parse-node next)
                                 (push child children)
                                 (setf next new-next)))
                      (let ((end-event (aref events next)))
                        (values (%make-format-edit-node
                                 :kind :sequence
                                 :start (%format-edit-event-offset event #'event-start-mark)
                                 :end (%format-edit-event-offset end-event #'event-end-mark)
                                 :anchor (sequence-start-event-anchor event)
                                 :style (sequence-start-event-style event)
                                 :children (nreverse children))
                                (1+ next)))))
                   ((mapping-start-event-p event)
                    (let ((pairs nil)
                          (next (1+ index)))
                      (loop until (mapping-end-event-p (aref events next))
                            do (multiple-value-bind (key after-key) (parse-node next)
                                 (multiple-value-bind (value after-value) (parse-node after-key)
                                   (push (%make-format-edit-pair :key key :value value) pairs)
                                   (setf next after-value))))
                      (let ((end-event (aref events next)))
                        (values (%make-format-edit-node
                                 :kind :mapping
                                 :start (%format-edit-event-offset event #'event-start-mark)
                                 :end (%format-edit-event-offset end-event #'event-end-mark)
                                 :anchor (mapping-start-event-anchor event)
                                 :style (mapping-start-event-style event)
                                 :pairs (nreverse pairs))
                                (1+ next)))))
                   (t
                    (%format-edit-structure-error nil "unexpected event while building edit tree"))))))
      (let ((index 0)
            (roots nil))
        (unless (and (< index (length events))
                     (stream-start-event-p (aref events index)))
          (%format-edit-structure-error nil "event stream has no stream start"))
        (incf index)
        (loop until (and (< index (length events))
                         (stream-end-event-p (aref events index)))
              do (unless (and (< index (length events))
                              (document-start-event-p (aref events index)))
                   (%format-edit-structure-error nil "event stream has an invalid document boundary"))
                 (incf index)
                 (multiple-value-bind (root after-root) (parse-node index)
                   (push root roots)
                   (setf index after-root))
                 (unless (and (< index (length events))
                              (document-end-event-p (aref events index)))
                   (%format-edit-structure-error nil "document has no end event"))
                 (incf index))
        (unless (< index (length events))
          (%format-edit-structure-error nil "event stream has no stream end"))
        (nreverse roots)))))

(defun %format-edit-proper-list-p (object)
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %format-edit-key-text (key)
  (if (stringp key) key (princ-to-string key)))

(defun %format-edit-map-pair (node key)
  (find-if (lambda (pair)
             (let ((key-node (format-edit-pair-key pair)))
               (and (eq (format-edit-node-kind key-node) :scalar)
                    (string= (%format-edit-key-text key)
                             (format-edit-node-value key-node)))))
           (format-edit-node-pairs node)))

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

(defun %format-edit-source-text (source)
  (cond
    ((stringp source)
     (values (if (simple-string-p source) source (coerce source 'simple-string)) nil))
    ((%format-edit-octet-p source)
     (multiple-value-bind (encoding bom-length)
         (%format-edit-source-encoding source)
       (let* ((bom (subseq source 0 bom-length))
              (body (subseq source bom-length))
              (text (cl-codec-kit:octets-to-string body
                                                   :encoding encoding :errorp t)))
         (values text
                 (lambda (edited)
                   (let ((encoded (cl-codec-kit:string-to-octets
                                   edited :encoding encoding)))
                     (if (plusp bom-length)
                         (concatenate '(vector (unsigned-byte 8)) bom encoded)
                         encoded)))))))
    (t
     (%format-edit-error 'yaml-format-edit-path-error nil 0 :set
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

(defun %format-edit-fragment (value)
  (string-trim '(#\Return #\Newline #\Space #\Tab)
               (emit value :default-flow-style :flow)))

(defun %format-edit-node-anchored-p (node)
  (or (format-edit-node-anchor node)
      (eq (format-edit-node-kind node) :alias)))

(defun %format-edit-check-safe-node (node path)
  (when (%format-edit-node-anchored-p node)
    (%format-edit-anchor-error path (format-edit-node-anchor node)
                               "anchor or alias is involved")))

(defun %format-edit-subtree-anchored-p (node)
  (or (%format-edit-node-anchored-p node)
      (case (format-edit-node-kind node)
        (:sequence (some #'%format-edit-subtree-anchored-p
                         (format-edit-node-children node)))
        (:mapping (some (lambda (pair)
                          (or (%format-edit-subtree-anchored-p
                               (format-edit-pair-key pair))
                              (%format-edit-subtree-anchored-p
                               (format-edit-pair-value pair))))
                        (format-edit-node-pairs node)))
        (otherwise nil))))

(defun %format-edit-find (root path)
  (let ((node root) (parent nil) (component nil) (ancestors nil))
    (dolist (part path)
      (push node ancestors)
      (when (%format-edit-node-anchored-p node)
        (%format-edit-anchor-error path (format-edit-node-anchor node)
                                   "path crosses an anchor or alias"))
      (case (format-edit-node-kind node)
        (:mapping
         (let ((pair (%format-edit-map-pair node part)))
           (unless pair (return-from %format-edit-find
                          (values nil node part (nreverse ancestors))))
           (let ((key (format-edit-pair-key pair)))
             (when (%format-edit-node-anchored-p key)
               (%format-edit-anchor-error path (format-edit-node-anchor key)
                                          "mapping key has an anchor or alias")))
           (setf parent node
                 component part
                 node (format-edit-pair-value pair))))
        (:sequence
         (unless (and (integerp part) (<= 0 part)
                      (< part (length (format-edit-node-children node))))
           (return-from %format-edit-find
             (values nil node part (nreverse ancestors))))
         (setf parent node
               component part
               node (nth part (format-edit-node-children node))))
        (:alias
         (%format-edit-anchor-error path (format-edit-node-anchor node)
                                    "path crosses an alias"))
        (otherwise
         (return-from %format-edit-find
           (values nil node component (nreverse ancestors))))))
    (values node parent component (nreverse ancestors))))

(defun %format-edit-check-ancestors (ancestors path)
  (dolist (node ancestors)
    (%format-edit-check-safe-node node path)))

(defun %format-edit-insertion-position (node text)
  (let ((position (format-edit-node-end node)))
    (unless (<= 0 position (length text))
      (%format-edit-structure-error nil "node end is outside source"))
    position))

(defun %format-edit-indent (node text)
  (let* ((start (format-edit-node-start node))
         (line-start (%format-edit-line-start text start)))
    (subseq text line-start start)))

(defun %format-edit-apply-replacement (text start end replacement)
  (concatenate 'simple-string (subseq text 0 start) replacement (subseq text end)))

(defun %format-edit-delete-entry (text parent pair-or-node path)
  (when (eq (format-edit-node-style parent) :flow)
    (%format-edit-structure-error path "deleting from a flow collection is not supported"))
  (let ((start (if (format-edit-pair-p pair-or-node)
                   (format-edit-node-start (format-edit-pair-key pair-or-node))
                   (format-edit-node-start pair-or-node)))
        (end (if (format-edit-pair-p pair-or-node)
                 (format-edit-node-end (format-edit-pair-value pair-or-node))
                 (format-edit-node-end pair-or-node))))
    (multiple-value-bind (start end) (%format-edit-block-span text start end)
      (values start end))))

(defun %format-edit-set-missing (text parent component value path document)
  (when (eq (format-edit-node-style parent) :flow)
    (%format-edit-structure-error path "adding to a flow collection is not supported"))
  (let ((fragment (%format-edit-fragment value))
        (newline (%format-edit-newline text))
        (indent (%format-edit-indent parent text))
        (position (%format-edit-insertion-position parent text)))
    (case (format-edit-node-kind parent)
      (:mapping
       (unless (or (stringp component) (numberp component) (characterp component))
         (%format-edit-error 'yaml-format-edit-path-error path document :set
                             "mapping key must be scalar"))
       (values position
               (concatenate 'simple-string
                            (if (and (plusp position)
                                     (not (member (char text (1- position))
                                                  '(#\Return #\Newline))))
                                newline "")
                            indent (%format-edit-fragment component) ": " fragment
                            (if (< position (length text)) newline ""))))
      (:sequence
       (unless (and (integerp component)
                    (= component (length (format-edit-node-children parent))))
         (%format-edit-error 'yaml-format-edit-path-error path document :set
                             "sequence edits only append at the next index"))
       (values position
               (concatenate 'simple-string
                            (if (and (plusp position)
                                     (not (member (char text (1- position))
                                                  '(#\Return #\Newline))))
                                newline "")
                            indent "- " fragment
                            (if (< position (length text)) newline ""))))
      (otherwise
       (%format-edit-error 'yaml-format-edit-path-error path document :set
                           "path parent is not a collection")))))

(defun edit-source (source path value &key (document 0) (operation :set))
  (unless (%format-edit-proper-list-p path)
    (%format-edit-error 'yaml-format-edit-path-error path document operation
                        "path must be a proper list"))
  (unless (and (integerp document) (<= 0 document))
    (%format-edit-error 'yaml-format-edit-path-error path document operation
                        "document must be a non-negative integer"))
  (unless (member operation '(:set :delete))
    (%format-edit-error 'yaml-format-edit-path-error path document operation
                        "operation must be :set or :delete"))
  (when (null path)
    (%format-edit-error 'yaml-format-edit-path-error path document operation
                        "path must not be empty"))
  (multiple-value-bind (text encoder) (%format-edit-source-text source)
    (let* ((events (parse-events source))
           (roots (%format-edit-node-from-events events))
           (root (nth document roots)))
      (unless root
        (%format-edit-error 'yaml-format-edit-path-error path document operation
                            "document does not exist"))
      (multiple-value-bind (node parent component ancestors)
          (%format-edit-find root path)
        (%format-edit-check-ancestors ancestors path)
        (cond
          (node
           (%format-edit-check-safe-node node path)
           (when (%format-edit-subtree-anchored-p node)
             (%format-edit-anchor-error path (format-edit-node-anchor node)
                                        "edited node contains an anchor or alias"))
           (when (eq operation :delete)
             (unless parent
               (%format-edit-error 'yaml-format-edit-path-error path document operation
                                   "cannot delete the document root"))
             (let ((entry (if (eq (format-edit-node-kind parent) :mapping)
                              (find-if (lambda (pair)
                                         (eq (format-edit-pair-value pair) node))
                                       (format-edit-node-pairs parent))
                              node)))
               (unless entry
                 (%format-edit-structure-error path "mapping entry is not in its parent"))
               (multiple-value-bind (start end)
                   (%format-edit-delete-entry text parent entry path)
                 (return-from edit-source
                   (if encoder
                       (funcall encoder (%format-edit-apply-replacement text start end ""))
                       (%format-edit-apply-replacement text start end ""))))))
           (let ((replacement (%format-edit-fragment value)))
             (if encoder
                 (funcall encoder
                          (%format-edit-apply-replacement text
                                                           (format-edit-node-start node)
                                                           (format-edit-node-end node)
                                                           replacement))
                 (%format-edit-apply-replacement text
                                                  (format-edit-node-start node)
                                                  (format-edit-node-end node)
                                                  replacement))))
          ((eq operation :delete)
           (%format-edit-error 'yaml-format-edit-path-error path document operation
                               "path does not exist"))
          (t
           (%format-edit-check-safe-node parent path)
           (%format-edit-check-ancestors (list parent) path)
           (when (%format-edit-subtree-anchored-p parent)
             (%format-edit-anchor-error path (format-edit-node-anchor parent)
                                        "edited collection contains an anchor or alias"))
           (multiple-value-bind (position insertion)
               (%format-edit-set-missing text parent component value path document)
             (let ((edited (%format-edit-apply-replacement text position position insertion)))
               (if encoder (funcall encoder edited) edited)))))))))
