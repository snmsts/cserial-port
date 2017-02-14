(in-package :common-lisp)

(defpackage :cserial-port
  (:use :cl)
  #-windows (:shadowing-import-from :iolib.syscalls :open :close :write :read)
  #-windows (:import-from :iolib.syscalls :defsyscall :o-rdwr :o-noctty :o-ndelay :getpgrp :pid-t :fcntl :f-setfl)
  (:import-from :cffi
		:with-foreign-object
		:with-foreign-slots
		:mem-aref)
  (:export :open-serial-port
           :close-serial-port
           :get-serial-port-state
           :read-serial-port-char
           :read-serial-port-byte
           :read-serial-port-string
           :serial-port-input-available-p
           :set-serial-port-state
           :wait-serial-port-state
           :write-serial-port-char
           :write-serial-port-string
           :write-serial-port-bytes))
