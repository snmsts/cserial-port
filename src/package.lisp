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
                :foreign-bitfield-symbols
                :foreign-bitfield-value
                :with-foreign-object
                :with-foreign-slots
                :mem-aref
                :mem-ref)
  (:export :open-serial
           :close-serial
           :get-serial-state
           :read-serial-char
           :read-serial-byte
           :read-serial-byte-vector
           :read-serial-string
           :serial-input-available-p
           :set-serial-state
           :wait-serial-state
           :write-serial-char
           :write-serial-byte
           :write-serial-string
           :write-serial-byte-vector
           :serial-stream
           :make-serial-stream
           :with-serial
           :with-timeout
           :timeout-error
           :serial-name
           :serial
           :output-available))
