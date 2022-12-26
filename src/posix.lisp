(cl:in-package :cserial-port)

(defcfun ("tcgetattr" tcgetattr) :int
  (fd  :int)
  (termios-p :pointer)) ;;struct termios *
(defcfun ("tcsetattr" tcsetattr) :int
  (fd  :int)
  (optional-actions  :int)
  (termios-p :pointer)) ;;struct termios *
(defcfun ("tcsetpgrp" tcsetpgrp) pid-t
  (fd  :int)
  (pgrp pid-t))
(defcfun ("tcsendbreak" tcsendbreak) :int
  (fd  :int)
  (duration :int))
(defcfun ("tcdrain" tcdrain) :int
  (fd  :int))
(defcfun ("tcflush" tcflush) :int
  (fd  :int)
  (queue_selector :int))
(defcfun ("tcflow" tcflow) :int
  (fd  :int)
  (action :int))
(defcfun ("cfsetispeed" cfsetispeed) :int
  (termios-p :pointer) ;;struct termios *
  (speed speed-t))
(defcfun ("cfsetospeed" cfsetospeed) :int
  (termios-p :pointer) ;;struct termios *
  (speed speed-t))
(defcfun ("ioctl" ioctl) :int
  (fd :int)
  (request :unsigned-long)
  (arg-p :pointer))

;; I'm not sure 'lognot' are available for this use or not. and in this case speed is not a matter at all.
(defun off (flag &rest patterns)
  (loop :for pattern :in patterns
     :do (decf flag (logand flag pattern)))
  flag)

(defclass posix-serial (serial)
  ((tty :initarg :tty
	:reader serial-tty
	:documentation "tty")
   (current-timeout-ds
    :initform nil
    :documentation "The current value of the timeout on the serial port, in
deci-seconds.")))

(defparameter *serial-class* 'posix-serial)

(defmethod %baud-rate ((s posix-serial) &optional baud-rate)
  (case (or baud-rate (serial-baud-rate s))
    ((0) B0)
    ((50) B50)
    ((75) B75)
    ((110) B110)
    ((134) B134)
    ((150) B150)
    ((200) B200)
    ((300) B300)
    ((600) B600)
    ((1200) B1200)
    ((2400) B2400)
    ((4800) B4800)
    ((9600) B9600)
    ((19200) B19200)
    ((38400) B38400)
    ((57600) B57600)
    ((115200) B115200)
    ((230400) B230400)
    ((460800) B460800)
    ((500000) B500000)
    ((576000) B576000)
    ((921600) B921600)
    ((1000000) B1000000)
    ((1152000) B1152000)
    ((1500000) B1500000)
    ((2000000) B2000000)
    ((2500000) B2500000)
    ((3000000) B3000000)
    ((3500000) B3500000)
    ((4000000) B4000000)

    (t (error "not supported baud rate ~A [bps]" baud-rate))))

(defmethod %data-bits ((s posix-serial) &optional data-bits)
  (let ((val (or data-bits (serial-data-bits s))))
    (case val
      ((5) CS5)
      ((6) CS6)
      ((7) CS7)
      ((8) CS8)
      (t (error "unsupported data-bits ~A" val)))))

(defmethod %stop-bits ((s posix-serial) &optional stop-bits)
  (let ((val (or stop-bits (serial-stop-bits s))))
    (case val
      (1 0)
      (2 CSTOPB)
      (t (error "unsupported stop bits ~A" val)))))

(defmethod %parity ((s posix-serial) &optional parity)
  (ecase (or parity (serial-parity s))
    (:none 0)
    (:even (logior PARENB))
    (:odd  (logior PARENB PARODD))
    (:mark (error "not supported mark"))
    (:space (error "not supported space"))))

(defmethod %valid-fd-p ((s posix-serial))
  (numberp (serial-fd s)))

(defmethod %set-invalid-fd ((s posix-serial))
  (setf (slot-value s 'fd) nil))

(defmethod %default-name ((s (eql 'posix-serial)) &optional (number 0))
  (format nil
	  (or #+linux  "/dev/ttyS~A"
	      #+freebsd "/dev/cuaa~A"
	      #+windows (if (> number 9)
			    "\\\\.\\COM~A"
			    "COM~A")
	      "/dont/know/where~A")
	  number))

(defmethod %close ((s posix-serial))
  (let ((fd (serial-fd s)))
    (fcntl fd f-setfl 0)
    (close fd))
  (%set-invalid-fd s)
  t)

(defmethod %open ((s posix-serial)
		  &key
		    name)
  (let* ((ratedef (%baud-rate s))
	 (fd (open name (logior o-rdwr o-noctty))))
    (when (= -1 fd)
      (error "~A open error!!" name))
    (setf (slot-value s 'fd) fd)
    (with-foreign-object (tty '(:struct termios))
      (unless (and
	       (zerop (tcgetattr fd tty))
	       (zerop (cfsetispeed tty ratedef))
	       (zerop (cfsetospeed tty ratedef)))
	(%close fd)
	(error "~A setspeed error!!" name))

      (with-foreign-slots ((lflag iflag cflag oflag cc) tty (:struct termios))
	(setf lflag (off lflag ICANON ECHO ECHONL IEXTEN ISIG))
	(setf iflag (off iflag BRKINT ICRNL INPCK ISTRIP IXON))
	(setf cflag (logior (off cflag PARENB CSTOPB CSIZE)
			    (%data-bits s)
			    (%parity s)
                            (%stop-bits s)
			    HUPCL CLOCAL))
	(setf oflag (off oflag OPOST))
	(setf (mem-aref cc 'cc-t VTIME) 0)
	(setf (mem-aref cc 'cc-t VMIN) 1))
      (unless (zerop (tcsetattr fd TCSANOW tty))
	(%close fd)
	(error "unable to setup serial port"))
      s)))

(defmethod %write ((s posix-serial) buffer write-size timeout-ms)
  (declare (ignorable timeout-ms)) ;; not supported yet
  (with-slots (fd) s
    ;;TODO: do something if return value is -1.
    (write fd buffer write-size)))

(defmethod %read ((s posix-serial) buffer buffer-size timeout-ms)
  (with-slots (fd current-timeout-ds) s
    ;; Use ceiling to ensure a value of 1 doesn't turn into 0 (no timeout)
    (let ((timeout-ds (unless (null timeout-ms) (ceiling (/ timeout-ms 100)))))
      (unless (eql timeout-ds current-timeout-ds)
        (flet ((signal-error ()
                 (error "Unable to set serial timeout")))
          ;; User has changed the timeout. Update it in foreign land and locally.
          (with-foreign-object (tty '(:struct termios))
            (unless (zerop (tcgetattr fd tty))
              (signal-error))
            (with-foreign-slots ((cc) tty (:struct termios))
              (let ((desired-vtime (if (null timeout-ds) 0 timeout-ds))
                    (desired-vmin (if (null timeout-ds) 1 0)))
                (setf (mem-aref cc 'cc-t VTIME) desired-vtime
                      (mem-aref cc 'cc-t VMIN) desired-vmin)
                ;; tcsetattr returns success if any change is made. So we need
                ;; to getattr again and make sure both values were set
                ;; appropriately.
                (unless (and (zerop (tcsetattr fd TCSANOW tty))
                             (zerop (tcgetattr fd tty))
                             (= desired-vtime (mem-aref cc 'cc-t VTIME))
                             (= desired-vmin (mem-aref cc 'cc-t VMIN)))
                  (signal-error))))))
        (setf current-timeout-ds timeout-ds)))
    (let ((count (read fd buffer buffer-size)))
      (when (and (zerop count)
                 (not (null timeout-ms)))
        (error 'timeout-error))
      count)))

(defmethod %get-serial-state ((s posix-serial) keys)
  (with-slots (fd) s
    (with-foreign-object (status :int)
      (unless (zerop (ioctl fd TIOCMGET status))
        (error "Unable to get serial state"))
      (let ((state (foreign-bitfield-symbols 'modem-state (mem-ref status :int))))
        (mapcar (lambda (entry) (when (member entry state) t)) keys)))))

(defmethod %set-serial-state ((s posix-serial)
                              &key
                                (dtr nil dtr-supplied-p)
                                (rts nil rts-supplied-p)
                                (break nil break-supplied-p))
  (declare (ignore break))
  (when break-supplied-p
    (error "BREAK not yet implemented"))
  (with-slots (fd) s
    (let ((bits-to-clear nil)
          (bits-to-set nil))
      (flet ((process-bit (name value set-p)
               (when set-p
                 (if value
                     (push name bits-to-set)
                     (push name bits-to-clear)))))
        (process-bit :dtr dtr dtr-supplied-p)
        (process-bit :rts rts rts-supplied-p)
        (unless (null bits-to-clear)
          (with-foreign-object (bits :int)
            (setf (mem-ref bits :int) (foreign-bitfield-value 'modem-state bits-to-clear))
            (unless (zerop (ioctl fd TIOCMBIC bits))
              (error "Unable to clear bits"))))
        (unless (null bits-to-set)
          (with-foreign-object (bits :int)
            (setf (mem-ref bits :int) (foreign-bitfield-value 'modem-state bits-to-set))
            (unless (zerop (ioctl fd TIOCMBIS bits))
              (error "Unable to set bits"))))))))

(defmethod %input-available-p ((s posix-serial))
  (with-slots (fd) s
    (with-foreign-object (nbytes :int)
      (unless (zerop (ioctl fd FIONREAD nbytes))
	(error "Unable to get number of bytes available"))
      (> (mem-ref nbytes :int) 0))))
