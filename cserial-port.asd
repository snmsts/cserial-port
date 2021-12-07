;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :cserial-port
  :description "library for serial communication inspired by lispworks' serial-port"
  :author "Masatoshi SANO <snmsts@gmail.com>"
  :version "0.0.3"
  :licence "MIT"
  :defsystem-depends-on (:trivial-features (:feature (:not :os-windows) :cffi-grovel))
  :depends-on (:trivial-features
               :trivial-gray-streams
               :cffi
               (:feature (:not :os-windows) :cffi-grovel)
               (:feature (:not :os-windows) :osicat))
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "interfaces")
             ;; Can switch to :IF-FEATURE once
             ;; https://gitlab.common-lisp.net/asdf/asdf/-/issues/63 is
             ;; addressed.
             #-windows
             ("cffi-grovel:grovel-file" "ffi-types" :pathname "ffi-types-unix")
             (:file "posix" :if-feature (:not :os-windows))
             (:file "win32" :if-feature :os-windows)
             (:file "main")
             (:file "gray"))))
  :serial t)
