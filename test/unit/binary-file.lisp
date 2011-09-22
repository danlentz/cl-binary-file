(in-package :common-lisp-user)

(defpackage :binary-file-tests
  (:use :common-lisp :lisp-unit :binary-file))

(in-package :binary-file-tests)

(define-test ensure-byte-array
  (assert-typep '(vector t 0) (binary-file::ensure-byte-array nil))
  (assert-true (array-has-fill-pointer-p (binary-file::ensure-byte-array nil)))
  (assert-eql 0 (fill-pointer (binary-file::ensure-byte-array nil)))
  (assert-true (adjustable-array-p (binary-file::ensure-byte-array nil)))
  (assert-typep '(vector t 3) (binary-file::ensure-byte-array '(1 2 3)))
  (assert-equalp #(1 2 3) (binary-file::ensure-byte-array '(1 2 3)))
  (assert-true (array-has-fill-pointer-p (binary-file::ensure-byte-array '(1 2 3))))
  (assert-eql 3 (fill-pointer (binary-file::ensure-byte-array '(1 2 3))))
  (assert-true (adjustable-array-p (binary-file::ensure-byte-array '(1 2 3))))
  (assert-typep '(vector t 3) (binary-file::ensure-byte-array #(1 2 3)))
  (assert-true (array-has-fill-pointer-p (binary-file::ensure-byte-array #(1 2 3))))
  (assert-eql 3 (fill-pointer (binary-file::ensure-byte-array #(1 2 3))))
  (assert-true (adjustable-array-p (binary-file::ensure-byte-array #(1 2 3))))
  (let ((arr (make-array 3 :adjustable t :fill-pointer t :initial-contents '(1 2 3))))
    (assert-typep '(vector t 3) (binary-file::ensure-byte-array arr))
    (assert-true (array-has-fill-pointer-p (binary-file::ensure-byte-array arr)))
    (assert-eql 3 (fill-pointer (binary-file::ensure-byte-array arr)))
    (assert-true (adjustable-array-p (binary-file::ensure-byte-array arr)))))

(define-test make-binary-array-input-stream
  (let ((in (make-binary-array-input-stream)))
    (assert-typep 'binary-array-input-stream in)
    (assert-eql 0 (file-position in))
    (assert-false (read-byte in nil nil)))
  (let ((in (make-binary-array-input-stream '(1 2 3))))
    (assert-typep 'binary-array-input-stream in)
    (assert-eql 0 (file-position in))
    (assert-eql 1 (read-byte in nil nil))
    (assert-eql 1 (file-position in))
    (assert-eql 2 (read-byte in nil nil))
    (assert-eql 2 (file-position in))
    (assert-eql 3 (read-byte in nil nil))
    (assert-eql 3 (file-position in))
    (assert-false (read-byte in nil nil))
    (file-position in :start)
    (assert-eql 0 (file-position in))
    (assert-eql 1 (read-byte in nil nil))
    (file-position in 1)
    (assert-eql 1 (file-position in))
    (assert-eql 2 (read-byte in nil nil))
    (file-position in :end)
    (assert-eql 3 (file-position in))
    (assert-false (read-byte in nil nil))))

(define-test make-binary-array-output-stream
  (let ((out (make-binary-array-output-stream)))
    (assert-typep 'binary-array-output-stream out)
    (assert-eql 0 (file-position out))
    (write-byte 1 out)
    (assert-eql 1 (file-position out))
    (assert-equalp #(1) (binary-array out))
    (write-byte 2 out)
    (assert-eql 2 (file-position out))
    (assert-equalp #(1 2) (binary-array out))
    (file-position out :start)
    (assert-eql 0 (file-position out))
    (write-byte 3 out)
    (assert-eql 1 (file-position out))
    (assert-equalp #(3 2) (binary-array out))

    (file-position out 1)
    (assert-eql 1 (file-position out))
    (write-byte 4 out)
    (assert-eql 2 (file-position out))
    (assert-equalp #(3 4) (binary-array out))

    (file-position out :end)
    (assert-eql 2 (file-position out))
    (write-byte 5 out)
    (assert-eql 3 (file-position out))
    (assert-equalp #(3 4 5) (binary-array out)))

  (let ((out (make-binary-array-output-stream '(1 2 3))))
    (assert-typep 'binary-array-output-stream out)
    (assert-eql 0 (file-position out))
    (write-byte 4 out)
    (assert-eql 1 (file-position out))
    (assert-equalp #(4 2 3) (binary-array out))))

(define-test make-binary-array-io-stream
  (let ((io (make-binary-array-io-stream '(1 2 3))))
    (assert-typep 'binary-array-io-stream io)
    (assert-eql 0 (file-position io))
    (assert-eql 1 (read-byte io nil nil))
    (assert-eql 1 (file-position io))
    (write-byte 4 io)
    (assert-eql 2 (file-position io))
    (assert-equalp #(1 4 3) (binary-array io))
    (file-position io 1)
    (assert-eql 4 (read-byte io nil nil))))

(define-test open-binary-stream
  (let ((stream (make-binary-array-io-stream)))
    (assert-eq stream (open-binary-stream stream))
    (close stream))
  (let ((stream (open-binary-stream "/tmp/binary-file-open-test.in" :if-exists :overwrite :if-does-not-exist :create)))
    (assert-typep 'stream stream)
    (assert-true (open-stream-p stream))
    (assert-equal #P"/tmp/binary-file-open-test.in" (pathname stream))
    (delete-file stream)
    (close stream)))

(define-test binary-stream-length
  (assert-eql 0 (binary-stream-length (make-binary-array-io-stream)))
  (assert-eql 3 (binary-stream-length (make-binary-array-io-stream '(1 2 3))))
  (let ((stream (open-binary-stream "/tmp/binary-file-open-test.in" :if-exists :overwrite :if-does-not-exist :create)))
    (assert-eql 0 (binary-stream-length stream))
    (delete-file stream)
    (close stream)))

(define-test binary-array-stream-p
  (assert-false (binary-array-stream-p nil))
  (assert-false (binary-array-stream-p 'foo))
  (assert-false (binary-array-stream-p "foo"))
  (assert-true (make-binary-array-io-stream '(1 2 3))))

(define-test twos-complement-signed
  (assert-eql  127 (binary-file::twos-complement-signed #b01111111 1))
  (assert-eql   64 (binary-file::twos-complement-signed #b01000000 1))
  (assert-eql    1 (binary-file::twos-complement-signed #b00000001 1))
  (assert-eql    0 (binary-file::twos-complement-signed #b00000000 1))
  (assert-eql   -1 (binary-file::twos-complement-signed #b11111111 1))
  (assert-eql  -64 (binary-file::twos-complement-signed #b11000000 1))
  (assert-eql -127 (binary-file::twos-complement-signed #b10000001 1))
  (assert-eql -128 (binary-file::twos-complement-signed #b10000000 1))

  (assert-eql  32767 (binary-file::twos-complement-signed #x7fff 2))
  (assert-eql  16384 (binary-file::twos-complement-signed #x4000 2))
  (assert-eql      1 (binary-file::twos-complement-signed #x0001 2))
  (assert-eql      0 (binary-file::twos-complement-signed #x0000 2))
  (assert-eql     -1 (binary-file::twos-complement-signed #xffff 2))
  (assert-eql -32767 (binary-file::twos-complement-signed #x8001 2))
  (assert-eql -32768 (binary-file::twos-complement-signed #x8000 2))

  (assert-eql  2147483647 (binary-file::twos-complement-signed #x7fffffff 4))
  (assert-eql  2147483646 (binary-file::twos-complement-signed #x7ffffffe 4))
  (assert-eql  1073741824 (binary-file::twos-complement-signed #x40000000 4))
  (assert-eql           1 (binary-file::twos-complement-signed #x00000001 4))
  (assert-eql           0 (binary-file::twos-complement-signed #x00000000 4))
  (assert-eql          -1 (binary-file::twos-complement-signed #xffffffff 4))
  (assert-eql          -2 (binary-file::twos-complement-signed #xfffffffe 4))
  (assert-eql          -4 (binary-file::twos-complement-signed #xfffffffc 4))
  (assert-eql          -8 (binary-file::twos-complement-signed #xfffffff8 4))
  (assert-eql         -16 (binary-file::twos-complement-signed #xfffffff0 4))
  (assert-eql -1073741824 (binary-file::twos-complement-signed #xc0000000 4))
  (assert-eql -2147483647 (binary-file::twos-complement-signed #x80000001 4))
  (assert-eql -2147483648 (binary-file::twos-complement-signed #x80000000 4)))

(define-test twos-complement
  (assert-eql #b11111111 (binary-file::twos-complement -1 1))
  (assert-eql #b11111110 (binary-file::twos-complement -2 1))
  (assert-eql #b10000000 (binary-file::twos-complement -128 1))

  (assert-eql 127 (binary-file::twos-complement #b01111111 1))
  (assert-eql 1 (binary-file::twos-complement 1 1))
  (assert-eql 0 (binary-file::twos-complement 0 1))
  (assert-eql -1 (binary-file::twos-complement #b11111111 1))
  (assert-eql -2 (binary-file::twos-complement #b11111110 1))
  (assert-eql -128 (binary-file::twos-complement #b10000000 1))

  (assert-eql #xffff (binary-file::twos-complement -1 2))
  (assert-eql #xfffe (binary-file::twos-complement -2 2))
  (assert-eql #x8000 (binary-file::twos-complement -32768 2))

  (assert-eql 32767 (binary-file::twos-complement #x7fff 2))
  (assert-eql 1 (binary-file::twos-complement 1 2))
  (assert-eql 0 (binary-file::twos-complement 0 2))
  (assert-eql -1 (binary-file::twos-complement #xffff 2))
  (assert-eql -2 (binary-file::twos-complement #xfffe 2))
  (assert-eql -32768 (binary-file::twos-complement #x8000 2)))

(define-test read-integer-basics
  (assert-eql 1 (read-integer (make-binary-array-io-stream '(1 2 3 4))))
  (assert-eql 1 (read-integer (make-binary-array-io-stream '(1 2 3 4)) :signed t))
  (assert-eql 1 (read-integer (make-binary-array-io-stream '(1 2 3 4)) :endianness :big-endian))
  (assert-eql 1 (read-integer (make-binary-array-io-stream '(1 2 3 4)) :signed t :endianness :big-endian))
  (assert-eql #x0201 (read-integer (make-binary-array-io-stream '(1 2 3 4)) :bytes 2))
  (assert-eql #x0102 (read-integer (make-binary-array-io-stream '(1 2 3 4)) :bytes 2 :endianness :big-endian)))

(define-test read-integer-with-file
  (with-open-file (out "test-file.bin" :direction :output :element-type 'unsigned-byte :if-exists :supersede :if-does-not-exist :create)
    (write-byte 0 out)
    (write-byte 1 out)
    (write-byte 2 out)
    (write-byte 3 out)
    (write-byte 4 out)
    (write-byte 5 out)
    (write-byte 6 out)
    (write-byte 7 out))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 0 (read-integer in))
    (assert-eql 1 (read-integer in))
    (assert-eql 2 (read-integer in))
    (assert-eql 3 (read-integer in)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 0 (read-integer in :signed t))
    (assert-eql 1 (read-integer in :signed t))
    (assert-eql 2 (read-integer in :signed t))
    (assert-eql 3 (read-integer in :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 0 (read-integer in :endianness :big-endian))
    (assert-eql 1 (read-integer in :endianness :big-endian))
    (assert-eql 2 (read-integer in :endianness :big-endian))
    (assert-eql 3 (read-integer in :endianness :big-endian)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 0 (read-integer in :endianness :big-endian :signed t))
    (assert-eql 1 (read-integer in :endianness :big-endian :signed t))
    (assert-eql 2 (read-integer in :endianness :big-endian :signed t))
    (assert-eql 3 (read-integer in :endianness :big-endian :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 256 (read-integer in :bytes 2))
    (assert-eql 770 (read-integer in :bytes 2)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 256 (read-integer in :bytes 2 :signed t))
    (assert-eql 770 (read-integer in :bytes 2 :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql   1 (read-integer in :bytes 2 :endianness :big-endian))
    (assert-eql 515 (read-integer in :bytes 2 :endianness :big-endian)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql   1 (read-integer in :bytes 2 :endianness :big-endian :signed t))
    (assert-eql 515 (read-integer in :bytes 2 :endianness :big-endian :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 50462976  (read-integer in :bytes 4))
    (assert-eql 117835012 (read-integer in :bytes 4)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 50462976  (read-integer in :bytes 4 :signed t))
    (assert-eql 117835012 (read-integer in :bytes 4 :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 66051  (read-integer in :bytes 4 :endianness :big-endian))
    (assert-eql 67438087 (read-integer in :bytes 4 :endianness :big-endian)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 66051  (read-integer in :bytes 4 :endianness :big-endian :signed t))
    (assert-eql 67438087 (read-integer in :bytes 4 :endianness :big-endian :signed t))))

(define-test read-integer-negative-values
  (with-open-file (out "test-file.bin" :direction :output :element-type 'unsigned-byte :if-exists :supersede :if-does-not-exist :create)
    (write-byte 255 out)
    (write-byte 254 out)
    (write-byte 253 out)
    (write-byte 252 out)
    (write-byte 251 out)
    (write-byte 250 out)
    (write-byte 249 out)
    (write-byte 248 out))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 255 (read-integer in))
    (assert-eql 254 (read-integer in))
    (assert-eql 253 (read-integer in))
    (assert-eql 252 (read-integer in)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql -1 (read-integer in :signed t))
    (assert-eql -2 (read-integer in :signed t))
    (assert-eql -3 (read-integer in :signed t))
    (assert-eql -4 (read-integer in :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 255 (read-integer in :endianness :big-endian))
    (assert-eql 254 (read-integer in :endianness :big-endian))
    (assert-eql 253 (read-integer in :endianness :big-endian))
    (assert-eql 252 (read-integer in :endianness :big-endian)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql -1 (read-integer in :endianness :big-endian :signed t))
    (assert-eql -2 (read-integer in :endianness :big-endian :signed t))
    (assert-eql -3 (read-integer in :endianness :big-endian :signed t))
    (assert-eql -4 (read-integer in :endianness :big-endian :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 65279 (read-integer in :bytes 2))
    (assert-eql 64765 (read-integer in :bytes 2)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql -257 (read-integer in :bytes 2 :signed t))
    (assert-eql -771 (read-integer in :bytes 2 :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 65534 (read-integer in :bytes 2 :endianness :big-endian))
    (assert-eql 65020 (read-integer in :bytes 2 :endianness :big-endian)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql -2 (read-integer in :bytes 2 :endianness :big-endian :signed t))
    (assert-eql -516 (read-integer in :bytes 2 :endianness :big-endian :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 4244504319 (read-integer in :bytes 4))
    (assert-eql 4177132283 (read-integer in :bytes 4)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql -50462977 (read-integer in :bytes 4 :signed t))
    (assert-eql -117835013 (read-integer in :bytes 4 :signed t)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql 4294901244 (read-integer in :bytes 4 :endianness :big-endian))
    (assert-eql 4227529208 (read-integer in :bytes 4 :endianness :big-endian)))

  (with-open-file (in "test-file.bin" :direction :input :element-type 'unsigned-byte)
    (assert-eql -66052 (read-integer in :bytes 4 :endianness :big-endian :signed t))
    (assert-eql -67438088 (read-integer in :bytes 4 :endianness :big-endian :signed t))))


(define-test read-integers
  (assert-equalp #(1) (read-integers (make-binary-array-io-stream '(1 2 3 4)) 1))
  (assert-equalp #(1 2) (read-integers (make-binary-array-io-stream '(1 2 3 4)) 2))
  (assert-equalp #(1 2 3) (read-integers (make-binary-array-io-stream '(1 2 3 4)) 3))
  (assert-equalp #(1 2 3 4) (read-integers (make-binary-array-io-stream '(1 2 3 4)) 4))
  (assert-equalp #(#x0201) (read-integers (make-binary-array-io-stream '(1 2 3 4)) 1 :bytes 2))
  (assert-equalp #(#x0201 #x0403) (read-integers (make-binary-array-io-stream '(1 2 3 4)) 2 :bytes 2)))


(define-test read-bytes
  (assert-equalp #(1) (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 1))
  (assert-equalp #(2) (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 1 1))
  (assert-equalp #(3) (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 1 2))
  (assert-equalp #(4) (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 1 3))
  (assert-equalp #(1 2 3 4) (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 4))
  (assert-equalp #(2 3 4) (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 3 1))
  (assert-error 'end-of-file (read-bytes (make-binary-array-io-stream '(1 2 3 4)) 4 1)))

(define-test read-array
  (assert-equalp #(1 2 3 4) (read-array (make-binary-array-io-stream '(1 2 3 4)) 4))
  (assert-equalp #(#\a #\b #\c #\d) (read-array (make-binary-array-io-stream '(97 98 99 100)) 4 #'read-char)))

(define-test int-bytes
  (assert-equalp '(0) (binary-file::int-bytes 0 1))
  (assert-equalp '(1) (binary-file::int-bytes 1 1))
  (assert-equalp '(255) (binary-file::int-bytes 255 1))
  (assert-equalp '(0 0) (binary-file::int-bytes 0 2))
  (assert-equalp '(0 1) (binary-file::int-bytes 256 2))
  (assert-equalp '(0 1) (binary-file::int-bytes 256 2)))

(define-test write-integer
  (let ((out (make-binary-array-io-stream)))
    (assert-equalp #(0) (write-integer 0 out))
    (assert-equalp #(0) (binary-array out))
    (assert-equalp #(1) (write-integer 1 out))
    (assert-equalp #(0 1) (binary-array out))
    (assert-equalp #(2 3) (write-integer #x0302 out :bytes 2))
    (assert-equalp #(0 1 2 3) (binary-array out))))

(define-test write-integer-big-endian
  (let ((out (make-binary-array-io-stream)))
    (assert-equalp #(3 2) (write-integer #x0302 out :bytes 2 :endianness :big-endian))
    (assert-equalp #(3 2) (binary-array out))))

(define-test write-integer-negative
  (let ((out (make-binary-array-io-stream)))
    (assert-equalp #(255) (write-integer -1 out))
    (assert-equalp #(255) (binary-array out))
    (assert-equalp #(128) (write-integer -128 out))
    (assert-equalp #(255 128) (binary-array out))
    (assert-equalp #(255 255) (write-integer -1 out :bytes 2))
    (assert-equalp #(255 128 255 255) (binary-array out))))

(define-test write-integer-with-condition
  (assert-error 'error (write-integer #x0302 (make-binary-array-io-stream))))

(define-test write-integers
  (let ((out (make-binary-array-io-stream)))
    (write-integers #(1 2 3 4) out)
    (assert-equalp #(1 2 3 4) (binary-array out))))

(define-test with-output-to-binary-array
  (assert-equalp #(1 2 3 4) (with-output-to-binary-array (s) (write-integers #(1 2 3 4) s))))

(define-test with-input-from-binary-array
  (assert-eql #x0201 (with-input-from-binary-array (s #(1 2)) (read-integer s :bytes 2))))

(define-test write/read-chars
  (let ((s (make-binary-array-io-stream)))
    (assert-false (read-char s nil nil))
    (print "foo" s)
    (fresh-line s)
    (file-position s 0)
    (assert-equalp "foo" (read s))))

(define-test stream-element-type
  (assert-eql :default (stream-element-type (make-binary-array-io-stream))))

(define-test peek-char
  (let ((s (make-binary-array-io-stream)))
    (assert-false (peek-char nil s nil nil))
    (assert-eql 0 (file-position s)))
  
  (let ((s (make-binary-array-io-stream)))
    (princ "abc" s)
    (file-position s 0)
    (assert-eql #\a (peek-char nil s))
    (assert-eql #\a (read-char s))))
    
  