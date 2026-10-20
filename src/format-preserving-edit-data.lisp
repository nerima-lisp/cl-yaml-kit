(in-package #:yaml-kit)

(defparameter *format-edit-byte-order-marks*
  '((:utf-32be . #(0 0 254 255))
    (:utf-32le . #(255 254 0 0))
    (:utf-8 . #(239 187 191))
    (:utf-16be . #(254 255))
    (:utf-16le . #(255 254))))

(defparameter *format-edit-crlf*
  (format nil "~C~C" #\Return #\Newline))

(defparameter *format-edit-line-feed-character* #\Newline)

(defparameter *format-edit-single-quote-character* #\')

(defparameter *format-edit-double-quote-character* #\")

(defparameter *format-edit-colon-character* #\:)

(defparameter *format-edit-question-character* #\?)

(defparameter *format-edit-sequence-indicator-character* #\-)

(defconstant +format-edit-bom-character+ (code-char #xfeff))
