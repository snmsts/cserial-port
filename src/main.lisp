(cl:in-package :cserial-port)

(defvar *default-name*
  (%default-name *serial-port-class*))

(defvar *default-baud-rate* 9600)
(defvar *default-encoding* :latin-1)
(defvar *default-data-bits* 8)
(defvar *default-stop-bits* 1)
(defvar *default-parity* :none)

;;interfaces borrowed from lisp works
(defun open-serial-port
    (&optional name 
     &rest args
     &key
     (baud-rate *default-baud-rate*)
     (encoding *default-encoding*)
     (data-bits *default-data-bits*)
     (stop-bits *default-stop-bits*)
     (parity *default-parity*)
     ;; below are not yet supported
     cts-flow-p
     dsr-flow-p
     dtr
     rts
     read-interval-timeout
     read-total-base-timeout
     read-total-byte-timeout
     write-total-base-timeout
     write-total-byte-timeout)
  "Attempts to open the named serial port and return a serial-port object."
  (setq name (let ((name (or name *default-name*)))
               (if (numberp name) (%default-name *serial-port-class* name) name)))
  (%open (apply #'make-instance *serial-port-class*
                :baud-rate baud-rate
                :databits data-bits
                :stopbits stop-bits
                :parity parity
                :encoding encoding
                args)
         :name name))

(defun close-serial-port (serial-port)
  "Closes a serial port"
  (unless (%valid-fd-p serial-port)
    (error "serial port ~S already closed" serial-port))
  (%close serial-port)
  t)

(defun get-serial-port-state (serial-port keys)
  "The function get-serial-port-state queries various aspects of the state of the serial port associated with serial-port .
The argument keys should be a list of one or more of the keywords :dsr and :cts . These cause get-serial-port-state to check the DSR and CTS lines respectively.
The result state is a list giving the state of each line in the same order as they appear in the argument keys ."
  ;;state
  (error "not yet implemented")
  nil)

(defun read-serial-port-char (serial-port &optional (timeout-error-p t) (timeout-char nil))
  "Reads a character from a serial port."
  (declare (ignorable timeout-char timeout-error-p)) ;;for now 
  (unless (%valid-fd-p serial-port)
    (error "invalid serial port ~S" serial-port))
  (cffi:with-foreign-object (b :unsigned-char 1)
    (let ((v (make-array
              20
              :fill-pointer 0
              :element-type '(unsigned-byte 8))))
      (loop :do
         (when (= (%read serial-port b 1) 1)
           (vector-push-extend (cffi:mem-aref b :unsigned-char) v)
           (let ((res (ignore-errors (babel:octets-to-string 
                                      v 
                                      :errorp t
                                      :encoding (serial-port-encoding serial-port)))))
             (when res
               (return-from read-serial-port-char
                 (aref res 0))))))
      nil)))

(defun read-serial-port-byte (serial-port &optional (timeout-error-p t))
  "Reads a byte from a serial port."
  (declare (ignorable timeout-error-p))
  (unless (%valid-fd-p serial-port)
    (error "invalid serial port ~S" serial-port))
  (cffi:with-foreign-object (b :unsigned-char 1)
    (when (= (%read serial-port b 1) 1)
      (cffi:mem-aref b :unsigned-char))))

(defun read-serial-port-byte-vector (buf serial-port &optional (timeout-error-p t) &key (start 0) (end (length buf)))
  "Reads a byte from a serial port."
  (declare (ignorable timeout-error-p))
  (unless (%valid-fd-p serial-port)
    (error "invalid serial port ~S" serial-port))
  (cffi:with-pointer-to-vector-data (buf-sap buf)
    (%read serial-port buf-sap (- end start))))

(defun read-serial-port-string (string serial-port &optional (timeout-error-p t) &key (start 0) (end nil))
  "Reads a string from a serial port."
  (loop :repeat (- (or end (length string)) start)
     :for i :from start
     :for nread :from 1
     :for c := (read-serial-port-char serial-port nil nil)
     :do (if c
             (setf (aref string i) c)
             (if timeout-error-p
                 (error "timeout")
                 (return-from read-serial-port-string (1- nread))))
     :finally (return-from read-serial-port-string nread))
  nil)

(defun serial-port-input-available-p (serial-port)
  "Checks whether a character is available on a serial port."   
  (%input-available-p serial-port))

(export
 (defun set-serial-port-state (serial-port &key dtr rts break)
   "Changes various aspects of the state of a serial port."
   t 
   "Description
The function set-serial-port-state changes various aspects of the state of the serial port associated with serial-port .
The argument dtr , if supplied, controls the DTR line. A true value means set and nil means clear. If dtr is not supplied, the state is unchanged.
The argument rts controls the RTS line in the same way.
The argument break controls the break state of the data line in the same way."
   (error "not yet implemented")
   nil
   ))

(export
 (defun wait-serial-port-state (serial-port keys &key timeout)
   "Waits for some aspect of the state of a serial port to change."
   t
   "Description
The function wait-serial-port-state waits for some state in the serial port associated with serial-port to change.
The argument keys should be a list of one or more of the keywords :cts , :dsr , :err , :ring , :rlsd and :break .
result is a list giving the keys for which the state has changed.
If timeout is non-nil then the function will return nil after that many seconds even if the state has not changed."
   (error "not yet implemented")
   nil))

(export 
 (defun write-serial-port-char (char serial-port &optional (timeout-error-p t))
   "Writes a character to a serial port."
   (write-serial-port-string (string char) serial-port timeout-error-p)
   char))

(export
 (defun write-serial-port-string (string serial-port &optional (timeout-error-p t) &key (start 0) (end nil))
   "Writes a string to a serial port."
   (declare (ignorable timeout-error-p))
   (unless (%valid-fd-p serial-port)
     (error "invalid serial port ~S" serial-port))
   (cffi:with-foreign-string ((b l) (subseq string start end) 
			      :encoding (serial-port-encoding serial-port))
     (%write serial-port b (1- l)))))

(export
 (defun write-serial-port-byte-vector (bytes serial-port &optional (timeout-error-p t) &key (start 0) (end (length bytes)))
   (declare (ignore timeout-error-p))
   (unless (%valid-fd-p serial-port)
     (error "invalid serial port ~S" serial-port))
   (cffi:with-pointer-to-vector-data (data-sap bytes)
     (%write serial-port data-sap (- end start)))))

;;more

(defmacro with-serial-port ((serial-port name &rest params) &body body)
  `(let ((,serial-port (open-serial-port ,name ,@params)))
     (unwind-protect 
	  (progn ,@body)
       (close-serial-port ,serial-port))))

;;test a bit

#+nil
(with-serial-port (rs2 #+windows 2 #-windows 0 :encoding :utf-8)
  (with-serial-port (rs1 1 :encoding :utf-8)
    (let* ((send (format nil "/-*~A~A~A"
                         (code-char 12354) #\cr #\lf))
           (recv (make-string (length send))))
      (write-serial-port-string send rs1)
      (sleep 1)
      (read-serial-port-string recv rs2)
      (print (list (equal send recv) recv)))
    (print :done)))
