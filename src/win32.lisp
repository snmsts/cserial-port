;; Copyright (c) 2012,  Blueswitch Pty Ltd.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the Blueswitch Pty Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL BLUESWITCH PTY LTD. BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cserial-port)

(defconstant +GENERIC_READ+  #x80000000)
(defconstant +GENERIC_WRITE+ #x40000000)
(defconstant +FILE_ATTRIBUTE_NORMAL+ #x80)
(defconstant +FILE_FLAG_OVERLAPPED+ #x40000000)
(defconstant +OPEN_EXISTING+ 3)

(defconstant +MAXDWORD+ 4294967295)

(defconstant +ONESTOPBIT+    0)
(defconstant +ONE5STOPBITS+  1)
(defconstant +TWOSTOPBITS+   2)

(defconstant +CBR_110+             110)
(defconstant +CBR_300+             300)
(defconstant +CBR_600+             600)
(defconstant +CBR_1200+            1200)
(defconstant +CBR_2400+            2400)
(defconstant +CBR_4800+            4800)
(defconstant +CBR_9600+            9600)
(defconstant +CBR_14400+           14400)
(defconstant +CBR_19200+           19200)
(defconstant +CBR_38400+           38400)
(defconstant +CBR_56000+           56000)
(defconstant +CBR_57600+           57600)
(defconstant +CBR_115200+          115200)
(defconstant +CBR_128000+          128000)
(defconstant +CBR_256000+          256000)

(defconstant +XON+ 17)
(defconstant +XOFF+ 19)

(defconstant +NOPARITY+            0)
(defconstant +ODDPARITY+           1)
(defconstant +EVENPARITY+          2)
(defconstant +MARKPARITY+          3)
(defconstant +SPACEPARITY+         4)

(defconstant +RTS_CONTROL_DISABLE+   0)
(defconstant +RTS_CONTROL_ENABLE+    1)
(defconstant +RTS_CONTROL_HANDSHAKE+ 2)
(defconstant +RTS_CONTROL_TOGGLE+    3)
(defconstant +SETRTS+                3)
(defconstant +CLRRTS+                4)

(defconstant +DTR_CONTROL_DISABLE+   0)
(defconstant +DTR_CONTROL_ENABLE+    1)
(defconstant +DTR_CONTROL_HANDSHAKE+ 2)
(defconstant +SETDTR+                5)
(defconstant +CLRDTR+                6)

(defconstant +INFINITE+ #xFFFFFFFF)

(defconstant +PURGE_TXCLEAR+ 4)
(defconstant +PURGE_TXABORT+ 1)
(defconstant +PURGE_RXCLEAR+ 8)
(defconstant +PURGE_RXABORT+ 2)

(cffi:load-foreign-library "kernel32")
  
(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype bool :uchar)

(cffi:defbitfield dcb-flags
  fBinary;     /* Binary Mode (skip EOF check)    */
  fParity;     /* Enable parity checking          */
  fOutxCtsFlow; /* CTS handshaking on output       */
  fOutxDsrFlow; /* DSR handshaking on output       */
  fDtrControl1;  /* DTR Flow control                */
  fDtrControl2;  /* DTR Flow control                */
  fDsrSensitivity; /* DSR Sensitivity              */
  fTXContinueOnXoff; /* Continue TX when Xoff sent */
  fOutX;       /* Enable output X-ON/X-OFF        */
  fInX;        /* Enable input X-ON/X-OFF         */
  fErrorChar;  /* Enable Err Replacement          */
  fNull;       /* Enable Null stripping           */
  fRtsControl1;  /* Rts Flow control                */
  fRtsControl2;  /* Rts Flow control                */
  fAbortOnError; /* Abort all reads and writes on Error */
)

(cffi:defcstruct dcb 
  (DCBlength dword);      /* sizeof(DCB)                     */
  (BaudRate dword);       /* Baudrate at which running       */
  (dcbflags dword);
  (wReserved word);       /* Not currently used              */
  (XonLim word);          /* Transmit X-ON threshold         */
  (XoffLim word);         /* Transmit X-OFF threshold        */
  (ByteSize :uint8);        /* Number of bits/byte, 4-8        */
  (Parity :uint8);          /* 0-4=None,Odd,Even,Mark,Space    */
  (StopBits :uint8);        /* 0,1,2 = 1, 1.5, 2               */
  (XonChar :char);         /* Tx and Rx X-ON character        */
  (XoffChar :char);        /* Tx and Rx X-OFF character       */
  (ErrorChar :char);       /* Error replacement char          */
  (EofChar :char);         /* End of Input character          */
  (EvtChar :char);         /* Received Event character        */
  (wReserved1 word));      /* Fill for now.                   */

(cffi:defcstruct commtimeouts
  (ReadIntervalTimeout dword)
  (ReadTotalTimeoutMultiplier dword)
  (ReadTotalTimeoutConstant dword)
  (WriteTotalTimeoutMultiplier dword)
  (WriteTotalTimeoutConstant dword))

(cffi:defcstruct comstat
  (fCtsHold dword)
  (fDsrHold  dword)
  (fRlsdHold dword)
  (fXoffHold dword)
  (fXoffSent dword)
  (fEof dword)
  (fTxim dword)
  (fReserved dword)
  (cbInQue dword)
  (cbOutQue dword))

(cffi:defctype pvoid (:pointer :void)) 
(cffi:defctype lpvoid (:pointer :void)) 
(cffi:defctype dword-ptr (:pointer dword))
(cffi:defctype ulong-ptr dword-ptr)
(cffi:defctype handle pvoid)
(cffi:defctype lpdword (:pointer dword))
(cffi:defctype lpword (:pointer word))
(cffi:defctype lpcomstat (:pointer comstat))
(cffi:defctype lpctstr :string)

(cffi:defcstruct overlapped-us
  (Offset dword)
  (OffsetHigh dword))

(cffi:defcunion overlapped-u
  (overlapped-us overlapped-us)
  (Pointer pvoid))

(cffi:defcstruct overlapped
  (Internal ulong-ptr)
  (InternalHigh ulong-ptr)
  (overlapped-u overlapped-u)
  (hEvent handle))

(cffi:defctype lpoverlapped (:pointer overlapped))

(cffi:defcstruct security-attributes
  (nLength dword)
  (lpSecurityDescriptor lpvoid)
  (bInheritHandle bool))

(cffi:defctype lpsecurity-attributes (:pointer security-attributes))


(cffi:defcfun (win32-reset-event "ResetEvent" :convention :stdcall) bool
  (hevent handle))

(cffi:defcfun (win32-clear-comm-error "ClearCommError" :convention :stdcall) bool
  (hfile handle)
  (lperrors lpdword)
  (lpstat lpcomstat))

(cffi:defcfun (win32-wait-for-single-object "WaitForSingleObject" :convention :stdcall) bool
  (hHandle handle)
  (dwMilliseconds dword))

(cffi:defcfun (win32-get-overlapped-result "GetOverlappedResult" :convention :stdcall) bool
  (hFile handle)
  (lpOverlapped lpoverlapped)
  (lpNumberOfBytesTransferred lpword)
  (bWait bool))
  
(cffi:defcfun (win32-purge-comm "PurgeComm" :convention :stdcall) bool
  (hFile handle)
  (flags dword))

(cffi:defcfun (win32-create-event "CreateEventA" :convention :stdcall) handle
  (lpEventAttributes lpsecurity-attributes)
  (bManualReset bool)
  (bInitialState bool)
  (lpName lpctstr))

(cffi:defcfun (win32-get-comm-timeouts "GetCommTimeouts" :convention :stdcall) bool
  (hFile handle)
  (timeouts (:pointer commtimeouts)))

(cffi:defcfun (win32-create-file "CreateFileA" :convention :stdcall) handle
  (filename :string)  
  (desired-access :uint32)  
  (share-mode :uint32) 
  (security-attribute :pointer)
  (creation-disposition :uint32)
  (flags-and-attributes :uint32) 
  (template-file :pointer))

(cffi:defcfun (win32-setup-comm "SetupComm" :convention :stdcall) bool
  (file :pointer)
  (dwInQueue dword)
  (dwOutQueue dword))

(cffi:defcfun (win32-escape-comm-function "EscapeCommFunction" :convention :stdcall) bool
  (file :pointer)
  (escape dword))

(cffi:defcfun (win32-set-comm-timeouts "SetCommTimeouts" :convention :stdcall) bool
  (file :pointer)
  (timeouts (:pointer commtimeouts)))

(cffi:defcfun (win32-set-comm-state "SetCommState" :convention :stdcall) bool
  (file :pointer)
  (dcb (:pointer dcb)))

(cffi:defcfun (win32-get-comm-state "GetCommState" :convention :stdcall) bool
  (file :pointer)
  (dcb (:pointer dcb)))

(cffi:defcfun (win32-memset "memset") :pointer
  (dest :pointer)
  (fill :int)
  (size :uint))
  
(cffi:defcfun (win32-close-handle "CloseHandle" :convention :stdcall) bool
  (object :pointer))

(cffi:defcfun (win32-read-file "ReadFile" :convention :stdcall) bool
  (file handle)
  (buffer :pointer)
  (size word)
  (readBytes (:pointer word))
  (overlapped :pointer))

(cffi:defcfun (win32-write-file "WriteFile" :convention :stdcall) bool
  (file :pointer)
  (buffer :pointer)
  (size word)
  (writtenBytes (:pointer word))
  (overlapped-p :pointer))

(cffi:defcfun (win32-get-last-error "GetLastError" :convention :stdcall) dword)

(defun valid-pointer-p (pointer)
  (not (cffi:pointer-eq pointer (cffi:make-pointer #xFFFFFFFF))))

(defmacro win32-confirm (form success fail)
  `(if (zerop ,form)
       (progn
	 (print (win32-get-last-error))
	 ,fail)
       ,success))

(defmacro win32-onerror (form &body error-form)
  `(win32-confirm ,form
		  t
		  (progn
		    ,@error-form)))

;;
(defparameter *serial-class* 'win32-serial)

(defclass win32-serial (serial)
  ())

(defmethod %baud-rate ((s win32-serial) &optional baud-rate)
  (case (or baud-rate (serial-baud-rate s))
    ((110) +CBR_110+)
    ((300) +CBR_300+)
    ((600) +CBR_600+)
    ((1200) +CBR_1200+)
    ((2400) +CBR_2400+)
    ((4800) +CBR_4800+)
    ((9600) +CBR_9600+)
    ((14400) +CBR_14400+)
    ((19200) +CBR_19200+)
    ((38400) +CBR_38400+)
    ((56000) +CBR_56000+)
    ((57600) +CBR_57600+)
    ((115200) +CBR_115200+)
    ((128000) +CBR_128000+)
    ((256000) +CBR_256000+)
    (t (error "not supported baud rate ~A [bps]" baud-rate))))

(defmethod %data-bits ((s win32-serial) &optional data-bits)
  (let ((val (or data-bits (serial-data-bits s))))
    (if (<= 4 val 8)
	val
	(error "unsupported data-bits ~A" val))))

(defmethod %stop-bits ((s win32-serial) &optional stop-bits)
  (let ((stop-bits (or stop-bits (serial-stop-bits s))))
    (cond
      ((= stop-bits 1) +ONESTOPBIT+)
      ((= stop-bits 1.5) +ONE5STOPBITS+)
      ((= stop-bits 2) +TWOSTOPBITS+)
      (t (error "unsupported stop-bits")))))

(defmethod %parity ((s win32-serial) &optional parity)
  (ecase (or parity (serial-parity s))
    (:none +NOPARITY+)
    (:even +EVENPARITY+)
    (:odd +ODDPARITY+)
    (:mark +MARKPARITY+)
    (:space +SPACEPARITY+)))

(defmethod %valid-fd-p ((s win32-serial))
  (let ((fd (serial-fd s)))
    (and (cffi:pointerp fd)
	 (valid-pointer-p fd)
	 t)))

(defmethod %set-invalid-fd ((s win32-serial))
  (setf (slot-value s 'fd) (cffi:make-pointer #xFFFFFFFF)))

(defmethod %default-name ((s (eql 'win32-serial)) &optional (number 1))
  (format nil (if (> number 9)
		  "\\\\.\\COM~A"
		  "COM~A") number))

(defmethod %close ((s win32-serial))
  (win32-close-handle (serial-fd s))
  (%set-invalid-fd s)
  t)

(defmethod %open ((s win32-serial)
                  &key
                    name)
  (let* ((null (cffi:null-pointer))
         (fd (win32-create-file name (logior +GENERIC_READ+ +GENERIC_WRITE+)
                                0 null +OPEN_EXISTING+ 0 null)))
    (unless (valid-pointer-p fd)
      (error "Create file invalid pointer"))
    (setf (slot-value s 'fd) fd)
    (cffi:with-foreign-object (ptr '(:struct dcb))
      (win32-memset ptr 0 (cffi:foreign-type-size '(:struct dcb)))
      (cffi:with-foreign-slots ((DCBlength) ptr (:struct dcb))
        (setf DCBlength (cffi:foreign-type-size '(:struct dcb))))
      (win32-onerror (win32-get-comm-state fd ptr)
                     (error "GetCommState failed"))
      (cffi:with-foreign-slots ((baudrate bytesize parity stopbits dcbflags)
                                ptr (:struct dcb))
        (setf baudrate (%baud-rate s))
        (setf bytesize (%data-bits s))
        (setf stopbits (%stop-bits s))
        (setf parity (%parity s))
        (setf dcbflags (cffi:foreign-bitfield-value 'dcb-flags '(fbinary))))
      (win32-onerror (win32-set-comm-state fd ptr)
                     (error "SetCommState failed"))))
  s)

(defmethod %write ((s win32-serial) buffer seq-size)
  (with-slots (fd) s
    (cffi:with-foreign-object (writtenbytes 'word)
      (win32-confirm
       (win32-write-file fd buffer seq-size writtenbytes (cffi:null-pointer))
       (cffi:mem-ref writtenbytes 'word)
       (error "could not write to device")))))

(defmethod %read ((s win32-serial) buf count)
  (with-slots (fd) s
    (cffi:with-foreign-object (readbytes 'word)
      (win32-confirm
       (win32-read-file fd buf count readbytes (cffi:null-pointer))
       (cffi:mem-ref readbytes 'word)
       (error "could not read from device")))))
