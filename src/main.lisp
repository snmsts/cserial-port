(cl:in-package :cserial-port)

(defvar *default-name*
  (%default-name *serial-class*))

(defvar *default-timeout-ms* nil)

(defvar *default-baud-rate* 9600)
(defvar *default-encoding* :latin-1)
(defvar *default-data-bits* 8)
(defvar *default-stop-bits* 1)
(defvar *default-parity* :none)

;;interfaces borrowed from lisp works
(defun open-serial
    (&optional name
     &rest args
     &key
     (baud-rate *default-baud-rate*)
     (encoding *default-encoding*)
     (data-bits *default-data-bits*)
     (stop-bits *default-stop-bits*)
     (parity *default-parity*)
     (cts-flow-p nil)
     ;; below are not yet supported
     dsr-flow-p
     dtr
     rts
     read-interval-timeout
     read-total-base-timeout
     read-total-byte-timeout
     write-total-base-timeout
     write-total-byte-timeout)
  "Attempts to open the named serial port and return a serial object."
  (setq name (let ((name (or name *default-name*)))
               (if (numberp name) (%default-name *serial-class* name) name)))
  (%open (apply #'make-instance *serial-class*
                :baud-rate baud-rate
                :data-bits data-bits
                :stop-bits stop-bits
                :parity parity
                :encoding encoding
                :cts-flow-p cts-flow-p
                args)
         :name name))

(defun close-serial (serial)
  "Closes a serial port"
  (unless (%valid-fd-p serial)
    (error "serial port ~S already closed" serial))
  (%close serial)
  t)

(defun get-serial-state (serial keys)
  "The function get-serial-state queries various aspects of the state of the serial port associated with serial .
The argument keys should be a list of one or more of the keywords :dsr and :cts . These cause get-serial-state to check the DSR and CTS lines respectively.
The result state is a list giving the state of each line in the same order as they appear in the argument keys ."
  (%get-serial-state serial keys))

(defun read-serial-char (serial &key (timeout-ms *default-timeout-ms*))
  "Reads a character from a serial port. will return a character."
  (unless (%valid-fd-p serial)
    (error "invalid serial port ~S" serial))
  (cffi:with-foreign-object (b :unsigned-char 1)
    (let ((v (make-array
              20
              :fill-pointer 0
              :element-type '(unsigned-byte 8))))
      (loop
         :do (when (= (%read serial b 1 timeout-ms) 1)
               (vector-push-extend (cffi:mem-aref b :unsigned-char) v)
               (let ((res (ignore-errors (babel:octets-to-string
                                          v
                                          :errorp t
                                          :encoding (serial-encoding serial)))))
                 (when res
                   (return (aref res 0)))))))))

(defun read-serial-byte (serial &key (timeout-ms *default-timeout-ms*))
  "Reads a byte from a serial port. will return byte."
  (unless (%valid-fd-p serial)
    (error "invalid serial port ~S" serial))
  (cffi:with-foreign-object (b :unsigned-char 1)
    (if (= (%read serial b 1 timeout-ms) 1)
        (cffi:mem-aref b :unsigned-char)
        :eof)))

(defun read-serial-byte-vector (buf serial &key (timeout-ms *default-timeout-ms*) (start 0) (end (length buf)))
  "Reads a byte from a serial port. will return count-read-bytes or nil when timeout."
  (unless (%valid-fd-p serial)
    (error "invalid serial port ~S" serial))
  (cffi:with-pointer-to-vector-data (buf-sap buf)
    (%read serial (cffi:inc-pointer buf-sap start) (- end start) timeout-ms)))

(defun read-serial-string (string serial &key (timeout-ms *default-timeout-ms*) (start 0) (end nil))
  "Reads a string from a serial port."
  (loop :repeat (- (or end (length string)) start)
     :for i :from start
     :for nread :from 1
     :do (setf (aref string i) (read-serial-char serial :timeout-ms timeout-ms))
     :finally (return nread)))

(defun serial-input-available-p (serial)
  "Checks whether a character is available on a serial port."
  (%input-available-p serial))

(defun set-serial-state (serial &rest args &key dtr rts break)
  "Changes various aspects of the state of a serial port."
  (declare (ignore dtr rts break))
  t
  "Description
The function set-serial-state changes various aspects of the state of the serial port associated with serial .
The argument dtr , if supplied, controls the DTR line. A true value means set and nil means clear. If dtr is not supplied, the state is unchanged.
The argument rts controls the RTS line in the same way.
The argument break controls the break state of the data line in the same way."
  (apply #'%set-serial-state serial args))

(defun wait-serial-state (serial keys &key (timeout-ms *default-timeout-ms*))
  "Waits for some aspect of the state of a serial port to change."
  t
  "Description
The function wait-serial-state waits for some state in the serial port associated with serial to change.
The argument keys should be a list of one or more of the keywords :cts , :dsr , :err , :ring , :rlsd and :break .
result is a list giving the keys for which the state has changed.
If timeout is non-nil then the function will return nil after that many seconds even if the state has not changed."
  (error "not yet implemented")
  nil)

(defun write-serial-char (char serial &key (timeout-ms *default-timeout-ms*))
  "Writes a character to a serial port. will return written char."
  (write-serial-string (string char) serial :timeout-ms timeout-ms))

(defun write-serial-string (string serial &key (timeout-ms *default-timeout-ms*) (start 0) (end nil))
  "Writes a string to a serial port. will return written-bytes count."
  (unless (%valid-fd-p serial)
    (error "invalid serial port ~S" serial))
  (cffi:with-foreign-string ((b l) (subseq string start end)
                             :encoding (serial-encoding serial))
    (%write serial b (1- l) timeout-ms)))

(defun write-serial-byte (byte serial &key (timeout-ms *default-timeout-ms*))
  "Writes a byte to a serial port. will return written byte."
  (let ((data (make-array 1
                          :element-type '(unsigned-byte 8)
                          :initial-contents (list byte))))
    (write-serial-byte-vector data serial :timeout-ms timeout-ms)
    byte))

(defun write-serial-byte-vector (bytes serial &key (timeout-ms *default-timeout-ms*) (start 0) (end (length bytes)))
  "Writes bytes to a serial port. will return written-bytes count."
  (unless (%valid-fd-p serial)
    (error "invalid serial port ~S" serial))
  (cffi:with-pointer-to-vector-data (data-sap bytes)
    (%write serial (cffi:inc-pointer data-sap start) (- end start) timeout-ms)))

;;more

(defmacro with-serial ((serial name &rest params) &body body)
  `(let ((,serial (open-serial ,name ,@params)))
     (unwind-protect
	  (progn ,@body)
       (close-serial ,serial))))

(defmacro with-timeout ((ms) &body body)
  `(let ((*default-timeout-ms* ,ms))
     ,@body))
