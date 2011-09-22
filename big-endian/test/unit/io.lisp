(in-package :common-lisp-user)

(defpackage :big-endian-tests
  (:use :common-lisp :lisp-unit :big-endian :binary-file))

(in-package :big-endian-tests)

(define-test write-int8
  (let ((out (make-binary-array-output-stream)))
    (write-int8 0 out)
    (write-int8 1 out)
    (write-int8 127 out)
    (write-int8 -1 out)
    (write-int8 -127 out)
    (write-int8 -128 out)
    (assert-equalp #(#b00000000
                     #b00000001
                     #b01111111
                     #b11111111
                     #b10000001
                     #b10000000) (binary-array out))))

(define-test read-int8
  (with-input-from-binary-array (in #(#b00000000
                                      #b00000001
                                      #b01111111
                                      #b11111111
                                      #b10000001
                                      #b10000000))
   (assert-eql 0 (read-int8 in))
   (assert-eql 1 (read-int8 in))
   (assert-eql 127 (read-int8 in))
   (assert-eql -1 (read-int8 in))
   (assert-eql -127 (read-int8 in))
   (assert-eql -128 (read-int8 in))
   (assert-error 'end-of-file (read-int8 in))))

(define-test write-uint8
  (let ((out (make-binary-array-output-stream)))
    (write-uint8 0 out)
    (write-uint8 1 out)
    (write-uint8 128 out)
    (write-uint8 255 out)
    (assert-equalp #(0 1 128 255) (binary-array out))))

(define-test read-uint8
  (with-input-from-binary-array (in #(0 1 128 255))
    (assert-eql 0 (read-uint8 in))
    (assert-eql 1 (read-uint8 in))
    (assert-eql 128 (read-uint8 in))
    (assert-eql 255 (read-uint8 in))
    (assert-error 'end-of-file (read-uint8 in))))

(define-test write-int16
  (let ((out (make-binary-array-output-stream)))
    (write-int16 0 out)
    (write-int16 1 out)
    (write-int16 127 out)
    (write-int16 -1 out)
    (write-int16 -127 out)
    (write-int16 -128 out)
    (assert-equalp #(#b00000000 #b00000000
                     #b00000000 #b00000001 
                     #b00000000 #b01111111 
                     #b11111111 #b11111111 
                     #b11111111 #b10000001 
                     #b11111111 #b10000000) (binary-array out))))

(define-test read-int16
  (with-input-from-binary-array (in #(#b00000000 #b00000000
                                      #b00000000 #b00000001
                                      #b00000000 #b01111111 
                                      #b11111111 #b11111111 
                                      #b11111111 #b10000001 
                                      #b11111111 #b10000000)) 
    (assert-eql 0 (read-int16 in))
    (assert-eql 1 (read-int16 in))
    (assert-eql 127 (read-int16 in))
    (assert-eql -1 (read-int16 in))
    (assert-eql -127 (read-int16 in))
    (assert-eql -128 (read-int16 in))
    (assert-error 'end-of-file (read-int16 in))))

(define-test write-uint16
  (let ((out (make-binary-array-output-stream)))
    (write-uint16 #x0000 out)
    (write-uint16 #x0001 out)
    (write-uint16 #x00ff out)
    (write-uint16 #xff00 out)
    (write-uint16 #xffff out)
    (assert-equalp #(#x00 #x00
                     #x00 #x01
                     #x00 #xff
                     #xff #x00
                     #xff #xff) (binary-array out))))

(define-test read-uint16
  (with-input-from-binary-array (in #(#x00 #x00
                                      #x00 #x01
                                      #x00 #xff
                                      #xff #x00
                                      #xff #xff))
    (assert-eql #x0000 (read-uint16 in))
    (assert-eql #x0001 (read-uint16 in))
    (assert-eql #x00ff (read-uint16 in))
    (assert-eql #xff00 (read-uint16 in))
    (assert-eql #xffff (read-uint16 in))
    (assert-error 'end-of-file (read-uint16 in))))

(define-test write-int32
  (let ((out (make-binary-array-output-stream)))
    (write-int32 #x00000000 out)
    (write-int32 #x00000001 out)
    (write-int32 #x000000ff out)
    (write-int32 #x0f000000 out)
    (write-int32 #x0fffffff out)
    (assert-equalp #(#x00 #x00 #x00 #x00
                     #x00 #x00 #x00 #x01
                     #x00 #x00 #x00 #xff
                     #x0f #x00 #x00 #x00
                     #x0f #xff #xff #xff)
                   (binary-array out))))

(define-test read-int32
  (with-input-from-binary-array (in #(#x00 #x00 #x00 #x00
                                      #x00 #x00 #x00 #x01
                                      #x00 #x00 #x00 #xff
                                      #x0f #x00 #x00 #x00
                                      #x0f #xff #xff #xff))
    (assert-eql #x00000000 (read-int32 in))
    (assert-eql #x00000001 (read-int32 in))
    (assert-eql #x000000ff (read-int32 in))
    (assert-eql #x0f000000 (read-int32 in))
    (assert-eql #x0fffffff (read-int32 in))
    (assert-error 'end-of-file (read-int32 in))))
                                    
(define-test write-uint32
  (let ((out (make-binary-array-output-stream)))
    (write-uint32 #x00000000 out)
    (write-uint32 #x00000001 out)
    (write-uint32 #x000000ff out)
    (write-uint32 #xff000000 out)
    (write-uint32 #xffffffff out)
    (assert-equalp #(#x00 #x00 #x00 #x00
                     #x00 #x00 #x00 #x01
                     #x00 #x00 #x00 #xff
                     #xff #x00 #x00 #x00
                     #xff #xff #xff #xff)
                   (binary-array out))))

(define-test read-uint32
  (with-input-from-binary-array (in #(#x00 #x00 #x00 #x00
                                      #x00 #x00 #x00 #x01
                                      #x00 #x00 #x00 #xff
                                      #xff #x00 #x00 #x00
                                      #xff #xff #xff #xff))
    (assert-eql #x00000000 (read-uint32 in))
    (assert-eql #x00000001 (read-uint32 in))
    (assert-eql #x000000ff (read-uint32 in))
    (assert-eql #xff000000 (read-uint32 in))
    (assert-eql #xffffffff (read-uint32 in))
    (assert-error 'end-of-file (read-uint32 in))))




