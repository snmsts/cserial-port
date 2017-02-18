;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

'#.(progn
     #+quicklisp
     (ql:quickload :trivial-features :silent t)
     #-quicklisp
     (asdf:load-system :trivial-features :verbose nil))

#-windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp
  (ql:quickload :cffi-grovel :silent t)
  #-quicklisp
  (asdf:load-system :cffi-grovel :verbose nil))

(cl:in-package :cl-user)

(asdf:defsystem :cserial-port
  :description "library for serial communication inspired by lispworks' serial-port"
  :author "Masatoshi SANO <snmsts@gmail.com>"
  :version "0.0.2"
  :licence "MIT"
  #-windows :defsystem-depends-on #-windows (:cffi-grovel)
  :depends-on (:trivial-features
               :trivial-gray-streams
               :cffi
               #-windows :cffi-grovel
               #-windows :osicat)
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
             (:file "main")
             (:file "gray"))))
  :serial t)
