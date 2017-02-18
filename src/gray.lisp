(in-package #:cserial-port)

(defclass serial-stream (trivial-gray-stream-mixin
                         fundamental-input-stream
                         fundamental-output-stream
                         fundamental-binary-stream)
  ((serial :initarg :serial
           :reader stream-serial)))

(defun make-serial-stream (serial)
  (make-instance 'serial-stream :serial serial))

(defmethod stream-element-type ((stream serial-stream))
  '(unsigned-byte 8))

(defmethod stream-read-byte ((stream serial-stream))
  (read-serial-byte (stream-serial stream)))

(defmethod stream-read-sequence ((stream serial-stream) sequence start end &key)
  (read-serial-byte-vector sequence (stream-serial stream) nil :start start :end end))

(defmethod stream-write-byte ((stream serial-stream) byte)
  (write-serial-byte byte (stream-serial stream))
  byte)

(defmethod stream-write-sequence ((stream serial-stream) sequence start end &key)
  (write-serial-byte-vector sequence (stream-serial stream) nil :start start :end end)
  sequence)

(defmethod stream-finish-output ((stream serial-stream))
  (close-serial (stream-serial stream)))
