(in-package :common-lisp-user)

;;(require :unit-test)

(defpackage binary-file
  (:use :cl);; :unit-test)
  (:export #:read-integer
           #:read-integers
           #:read-array
           #:read-bytes
           #:write-integer
           #:write-integers
           #:open-binary-stream
           #:binary-stream-length
           #:binary-array
           #:binary-array-stream
           #:binary-array-stream-p
           #:binary-array-input-stream
           #:binary-array-output-stream
           #:binary-array-io-stream
           #:make-binary-array-io-stream
           #:make-binary-array-input-stream
           #:make-binary-array-output-stream
           #:with-output-to-binary-array
           #:with-input-from-binary-array)
  (:documentation "BINARY-FILE

  The binary file package contains utilities to read and write binary
  files.  The utilities support writing bytes of size 8 bits (octets)
  and currently only two's complement signed encoding is supported.

  The package has also in-memory binary stream which can be used as a
  file stream."))
