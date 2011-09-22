(in-package :little-endian)

(defun write-int8 (value stream)
  (declare (type (signed-byte 8) value))
  (binary-file:write-integer value stream :bytes 1))

(defun write-uint8 (value stream)
  (declare (type (unsigned-byte 8) value))
  (binary-file:write-integer value stream :bytes 1))

(defun write-int16 (value stream)
  (declare (type (signed-byte 16) value))
  (binary-file:write-integer value stream :bytes 2))

(defun write-uint16 (value stream)
  (declare (type (unsigned-byte 16) value))
  (binary-file:write-integer value stream :bytes 2))

(defun write-int32 (value stream)
  (declare (type (signed-byte 32) value))
  (binary-file:write-integer value stream :bytes 4))

(defun write-uint32 (value stream)
  (declare (type (unsigned-byte 32) value))
  (binary-file:write-integer value stream :bytes 4))

(defun read-int8 (stream)
  (binary-file:read-integer stream :bytes 1 :signed t))

(defun read-uint8 (stream)
  (binary-file:read-integer stream :bytes 1))

(defun read-int16 (stream)
  (binary-file:read-integer stream :bytes 2 :signed t))

(defun read-uint16 (stream)
  (binary-file:read-integer stream :bytes 2))

(defun read-int32 (stream)
  (binary-file:read-integer stream :bytes 4 :signed t))

(defun read-uint32 (stream)
  (binary-file:read-integer stream :bytes 4))

(defun read-uint64 (stream)
  (binary-file:read-integer stream :bytes 8))
