(in-package #:cserial-port)

(defclass serial-port-stream (fundamental-input-stream
                              fundamental-output-stream
                              fundamental-binary-stream)
  ((serial-port :initarg :serial-port
                :reader stream-serial-port)))

(defun make-serial-port-stream (serial-port)
  (make-instance 'serial-port-stream :serial-port serial-port))

(defmethod stream-element-type ((stream serial-port-stream))
  '(unsigned-byte 8))

(defmethod stream-read-byte ((stream serial-port-stream))
  (read-serial-port-byte (stream-serial-port stream)))

(defmethod stream-read-sequence ((stream serial-port-stream) sequence start end &key)
  (read-serial-port-byte-vector sequence (stream-serial-port stream) nil :start start :end end))

(defmethod stream-write-byte ((stream serial-port-stream) byte)
  (write-serial-port-byte byte (stream-serial-port stream))
  byte)

(defmethod stream-write-sequence ((stream serial-port-stream) sequence start end &key)
  (write-serial-port-byte-vector sequence (stream-serial-port stream) nil :start start :end end)
  sequence)

(defmethod stream-finish-output ((stream serial-port-stream))
  (close-serial-port (stream-serial-port stream)))
