(in-package :common-lisp)

(defpackage :cserial-port
  (:use :cl
        :trivial-gray-streams)
  #-windows
  (:shadowing-import-from :osicat-posix :open :close :write :read)
  #-windows
  (:import-from :osicat-posix :o-rdwr :o-noctty :o-ndelay :getpgrp :fcntl :f-setfl)
  (:import-from :cffi
                :defcfun
                :with-foreign-object
                :with-foreign-slots
                :mem-aref)
  (:export :open-serial-port
           :close-serial-port
           :get-serial-port-state
           :read-serial-port-char
           :read-serial-port-byte
           :read-serial-port-byte-vector
           :read-serial-port-string
           :serial-port-input-available-p
           :set-serial-port-state
           :wait-serial-port-state
           :write-serial-port-char
           :write-serial-port-byte
           :write-serial-port-string
           :write-serial-port-byte-vector
           :serial-port-stream
           :make-serial-port-stream))
