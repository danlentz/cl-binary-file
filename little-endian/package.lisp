(defpackage binary-file.little-endian
  (:use :cl :binary-file) ;; :unit-test
  (:nicknames :little-endian)
  (:export #:write-uint8
           #:write-uint16
           #:write-uint32
           #:write-int8
           #:write-int16
           #:write-int32

           #:read-uint8
           #:read-uint16
           #:read-uint32
           #:read-uint64
           #:read-int8
           #:read-int16
           #:read-int32))
