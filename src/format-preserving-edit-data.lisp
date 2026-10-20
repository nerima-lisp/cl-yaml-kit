(in-package #:yaml-kit)

(defparameter *format-edit-byte-order-marks*
  '((:utf-32be . #(0 0 254 255))
    (:utf-32le . #(255 254 0 0))
    (:utf-8 . #(239 187 191))
    (:utf-16be . #(254 255))
    (:utf-16le . #(255 254))))

(defparameter *format-edit-crlf*
  (format nil "~C~C" #\Return #\Newline))

(defconstant *format-edit-line-feed-character* #\Newline)

(defconstant *format-edit-single-quote-character* #\')

(defconstant *format-edit-double-quote-character* #\")

(defconstant *format-edit-colon-character* #\:)

(defconstant *format-edit-question-character* #\?)

(defconstant *format-edit-sequence-indicator-character* #\-)

(defconstant +format-edit-bom-character+ (code-char #xfeff))
