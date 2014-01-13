;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#-windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(cl:in-package :cl-user)

(asdf:defsystem :cserial-port
  :description "library for serial communication inspired by lispworks' serial-port"
  :author "Masatoshi SANO <snmsts@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :depends-on (:trivial-features 
               :cffi 
               #-windows :cffi-grovel
               #-windows :iolib.syscalls)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "interfaces")
             #-windows
             (cffi-grovel:grovel-file "ffi-types" :pathname 
                                      "ffi-types-unix")
             #-windows
             (:file "posix")
             #+windows
             (:file "win32")
             (:file "main"))))
  :serial t)
