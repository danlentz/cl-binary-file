(in-package :big-endian)

(defun write-int8 (value stream)
  (declare (type (signed-byte 8) value))
  (binary-file:write-integer value stream :bytes 1 :endianness :big-endian))

(defun write-uint8 (value stream)
  (declare (type (unsigned-byte 8) value))
  (binary-file:write-integer value stream :bytes 1 :endianness :big-endian))

(defun write-int16 (value stream)
  (declare (type (signed-byte 16) value))
  (binary-file:write-integer value stream :bytes 2 :endianness :big-endian))

(defun write-uint16 (value stream)
  (declare (type (unsigned-byte 16) value))
  (binary-file:write-integer value stream :bytes 2 :endianness :big-endian))

(defun write-int32 (value stream)
  (declare (type (signed-byte 32) value))
  (binary-file:write-integer value stream :bytes 4 :endianness :big-endian))

(defun write-uint32 (value stream)
  (declare (type (unsigned-byte 32) value))
  (binary-file:write-integer value stream :bytes 4 :endianness :big-endian))

(defun read-int8 (stream)
  (binary-file:read-integer stream :bytes 1 :signed t :endianness :big-endian))

(defun read-uint8 (stream)
  (binary-file:read-integer stream :bytes 1 :endianness :big-endian))

(defun read-int16 (stream)
  (binary-file:read-integer stream :bytes 2 :signed t :endianness :big-endian))

(defun read-uint16 (stream)
  (binary-file:read-integer stream :bytes 2 :endianness :big-endian))

(defun read-int32 (stream)
  (binary-file:read-integer stream :bytes 4 :signed t :endianness :big-endian))

(defun read-uint32 (stream)
  (binary-file:read-integer stream :bytes 4 :endianness :big-endian))

(defun read-uint64 (stream)
  (binary-file:read-integer stream :bytes 8 :endianness :big-endian))
