# cserial-port

[![Build Status](https://travis-ci.org/snmsts/cserial-port.svg?branch=master)](https://travis-ci.org/snmsts/cserial-port)
[![Build status](https://ci.appveyor.com/api/projects/status/nq0k756e7baeh5gy?svg=true)](https://ci.appveyor.com/project/snmsts/cserial-port)


Common Lisp library for interacting with serial ports.

## Usage

```common-lisp
(with-serial (rs1 1)
  (write-serial-byte-vector
    (babel:string-to-octets "Hello")
    rs1))

;; Interacting with 2 serial ports.
(with-serial (rs2 2)
  (with-serial (rs1 1)

    (write-serial-byte-vector
      (babel:string-to-octets "こんにちは。")
      rs1)

    (let ((res (make-array 18 :element-type '(unsigned-byte 8))))
      (read-serial-sequence res rs2))))

;; Using a gray-stream interface
(with-serial (rs1 1)
  (let ((stream (make-serial-stream rs1)))
    ;; Allow to use write/read-sequence.
    (write-sequence 
      ;; Sending 'Hi'
      (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(72 105))
      stream)))
```
