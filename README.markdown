# cserial-port

Common Lisp library for interacting with serial ports.

## Usage

```common-lisp
(with-serial-port (rs1 1)
  (write-serial-port-byte-vector
    (babel:string-to-octets "Hello")
    rs1))

;; Interacting with 2 serial ports.
(with-serial-port (rs2 2)
  (with-serial-port (rs1 1)

    (write-serial-port-byte-vector
      (babel:string-to-octets "こんにちは。")
      rs1)

    (let ((res (make-array 18 :element-type '(unsigned-byte 8))))
      (read-serial-port-sequence res rs2))))

;; Using a gray-stream interface
(with-serial-port (rs1 1)
  (let ((stream (make-serial-port-stream rs1)))
    ;; Allow to use write/read-sequence.
    (write-sequence 
      ;; Sending 'Hi'
      (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(72 105))
      stream)))
```
