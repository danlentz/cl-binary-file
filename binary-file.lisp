(in-package :binary-file)

(defclass binary-array-stream (trivial-gray-streams:fundamental-binary-stream trivial-gray-streams:fundamental-character-stream)
  ((binary-array :initarg :array :reader binary-array)
   (position :initform 0 :accessor binary-array-position))
  (:default-initargs :array :nil)
  (:documentation "The base class for binary array streams. The stream
is implemented with gray streams. The stream can used to store
both characters and bytes. The class has adjustable array for storing
stream contents."))
  
(defclass binary-array-output-stream (binary-array-stream trivial-gray-streams:fundamental-binary-output-stream trivial-gray-streams:fundamental-character-output-stream)
  ()
  (:documentation "The binary array output stream for writing data to binary stream."))

(defclass binary-array-input-stream (binary-array-stream trivial-gray-streams:fundamental-binary-input-stream trivial-gray-streams:fundamental-character-input-stream)
  ()
  (:documentation "The binary array input stream for reading data from binary stream."))

(defclass binary-array-io-stream (binary-array-output-stream binary-array-input-stream)
  ()
  (:documentation "The binary array input/output stream for both reading and writng data from and to binary stream."))

;;(defmethod trivial-gray-streams:stream-line-length ((stream binary-array-output-stream))
;;  nil)

(defmethod trivial-gray-streams:stream-line-column ((stream binary-array-output-stream))
  nil)

(defmethod trivial-gray-streams:stream-start-line-p ((stream binary-array-output-stream))
  nil)

(defmethod trivial-gray-streams:stream-write-char ((stream binary-array-output-stream) ch)
  (trivial-gray-streams:stream-write-byte stream (char-code ch)))

(defmethod trivial-gray-streams:stream-read-char ((stream binary-array-input-stream))
  (let ((code (trivial-gray-streams:stream-read-byte stream)))
    (if (eql code :eof)
        :eof
        (code-char code))))

(defmethod trivial-gray-streams:stream-unread-char ((stream binary-array-input-stream) ch)
  (when (> (binary-array-position stream) 0)
    (decf (binary-array-position stream)))
  nil)

(defmethod trivial-gray-streams::stream-element-type ((stream binary-array-stream))
  :default)

(defmethod trivial-gray-streams:stream-read-byte ((stream binary-array-input-stream))
  (if (< (binary-array-position stream) (fill-pointer (binary-array stream)))
      (prog1 (aref (binary-array stream) (binary-array-position stream))
        (incf (binary-array-position stream)))
      :eof))

(defmethod trivial-gray-streams:stream-write-byte ((stream binary-array-output-stream) integer)
  (unless (array-in-bounds-p (binary-array stream) (binary-array-position stream))
    (adjust-array (binary-array stream) (1+ (max (* (array-dimension (binary-array stream) 0) 2)
                                                 (binary-array-position stream)
                                                 1))))
  (prog1 (setf (aref (binary-array stream) (binary-array-position stream)) integer)
    (when (>= (binary-array-position stream) (fill-pointer (binary-array stream)))
      (setf (fill-pointer (binary-array stream)) (1+ (binary-array-position stream))))
    (incf (binary-array-position stream))))

#+sbcl(defmethod sb-gray:stream-file-position ((stream binary-array-stream) &optional position-spec)
        "Returns or sets stream file position. If position-spec is given the stream file position is set accordingly.
position-spec :start sets file position in the beginning of the
stream, :end to the end of stream and integer value to according
offset."
  (if (null position-spec)
      (binary-array-position stream)
      (case position-spec
        (:start (setf (binary-array-position stream) 0))
        (:end (setf (binary-array-position stream) (fill-pointer (binary-array stream))))
        (t (setf (binary-array-position stream) position-spec)))))

(defmethod trivial-gray-streams:stream-file-position ((stream binary-array-stream))
        "Returns file position."
        (binary-array-position stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream binary-array-stream))
        "Sets stream file position. position-spec :start sets file
position in the beginning of the stream, :end to the end of stream and
integer value to according offset."
  (case position-spec
    (:start (setf (binary-array-position stream) 0))
    (:end (setf (binary-array-position stream) (fill-pointer (binary-array stream))))
    (t (setf (binary-array-position stream) position-spec))))

(defun binary-stream-length (stream)
  "Returns length of stream."
  (if (binary-array-stream-p stream)
      (fill-pointer (binary-array stream))
      (file-length stream)))

(defmethod close ((stream binary-array-stream) &key abort)
  "Closes stream."
  (declare (ignore stream abort))
  t)

(defun ensure-byte-array (input-byte-seq)
  "Returns byte array suitable for binary array stream. input-byte-seq is sequence of bytes
which is converted to suitable byte array if necessary."
  (typecase input-byte-seq
    (array 
     (if (and (array-has-fill-pointer-p input-byte-seq)
              (adjustable-array-p input-byte-seq))
         input-byte-seq
         (make-array (array-dimension input-byte-seq 0)
                     :element-type 'unsigned-byte
                     :initial-contents (map 'list #'identity input-byte-seq)
                     :adjustable t
                     :fill-pointer t)))
    (list (make-array (length input-byte-seq) :element-type 'unsigned-byte :initial-contents input-byte-seq :adjustable t :fill-pointer t))))

(defun make-binary-array-input-stream (&optional initial-contents)
  "Returns new binary array input stream. The initial-contents is
sequence of bytes and if given the stream contains bytes in
initial-contents."
  (make-instance 'binary-array-input-stream :array (ensure-byte-array initial-contents)))

(defun make-binary-array-output-stream (&optional initial-contents)
  "Returns new binary array output stream. The initial-contents is
sequence of bytes and if given the stream contains bytes in
initial-contents."
  (make-instance 'binary-array-output-stream :array (ensure-byte-array initial-contents)))

(defun make-binary-array-io-stream (&optional initial-contents)
  "Returns new binary array input/output stream. The initial-contents is
sequence of bytes and if given the stream contains bytes in
initial-contents."
  (make-instance 'binary-array-io-stream :array (ensure-byte-array initial-contents)))

(defgeneric binary-array-stream-p (obj)
  (:documentation "Returns t, if obj is binary-array-stream, nil otherwise.")
  (:method ((obj binary-array-stream)) t)
  (:method (obj) nil))

(defun twos-complement-signed (value bytes &aux (bits (* bytes 8)) (sign-bit (1- bits)))
  "Returns signed integer. Value is in two's complement format and
result has bytes * 8 bits."
  (if (logbitp sign-bit value) ; if sign bit is set convert it to negative
      (1- (- (logandc2 (1- (ash 1 bits)) value))) 
                                        ; (1+ (lognot value)) won't work because lisp
                                        ; sees result as integer of size bits+1.
                                        ; So the conversion is done is a way that lisp understands the type
                                        ; and signess of the value.
      value))

(defun twos-complement (value bytes)
  "Returns two's complement of given value. Bytes is size of value in
bytes."
  (if (<= value 0)
      (ldb (byte (* 8 bytes) 0) value)
      (twos-complement-signed value bytes)))
  

(defun read-integer (stream &key (bytes 1) (signed nil) (endianness :little-endian))  
  "Reads and returns integer from stream. Integer size is (8 * bytes)
bits. If signed is t integer is interpreted as signed, otherwise
unsigned. Byte order is defined by endianness."
  (case endianness
    (:little-endian
     (do ((byte 0 (1+ byte))
          (shift 0 (+ shift 8))
          (value 0))
         ((>= byte bytes) 
          (if signed
              (twos-complement-signed value bytes)
              value))
       (setq value (logior value (ash (read-byte stream) shift)))))
    (:big-endian
     (do ((byte 0 (1+ byte))
          (shift (* (1- bytes) 8) (- shift 8))
          (value 0))
         ((>= byte bytes)
          (if signed
              (twos-complement-signed value bytes)
              value))
       (setq value (logior value (ash (read-byte stream) shift)))))))

(defun read-integers (stream size &key (bytes 1) (signed nil) (endianness :little-endian))
  "Reads and returns array of integers of size bytes."
  (let ((seq (make-array size :element-type 'integer :initial-element 0)))
    (dotimes (i size)
      (setf (aref seq i) (read-integer stream :bytes bytes :signed signed :endianness endianness)))
    seq))

(defun read-bytes (stream size &optional offset)
  "Reads and returns size bytes from stream. If offset is given,
stream is positioned to that offset before reading."
  (let ((seq (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    (when offset
      (file-position stream offset))
    (when (< (read-sequence seq stream) (length seq))
      (error 'end-of-file :stream stream))
    seq))

(defun read-array (stream size &optional (element-reader #'read-byte))
  "Reads and returns array of values. Each value is read with element-reader."
  (let ((seq (make-array size :element-type 'integer :initial-element 0)))
    (dotimes (i size)
      (setf (aref seq i) (funcall element-reader stream)))
    seq))

(defun int-bytes (int bytes &aux (bytes-1 (1- bytes)))
  "Returns bytes from given (positive) integer least significant byte first."
  (if (<= bytes-1 0)
      (cons int nil)
      (cons (logand #xff int) (int-bytes (ash int -8) bytes-1))))

(defun write-integer (value stream &key (bytes 1) (endianness :little-endian))  
  "Writes integer to stream as unsigned bytes. Byte order is defined by endianness."
  (when (> (/ (integer-length value) 8.0) bytes)
    (error "Value ~D is too big to fit in ~D bytes." value bytes))

  (let ((uint-value value))
    (when (minusp value)
      (setq uint-value (twos-complement value bytes)))
    (case endianness
      (:little-endian
       (write-sequence (make-array bytes :initial-contents (int-bytes uint-value bytes)) stream))
      (:big-endian
       (write-sequence (make-array bytes :initial-contents (reverse (int-bytes uint-value bytes))) stream)))))

(defun write-integers (seq stream &key (bytes 1) (endianness :little-endian))
  "Write sequence of integers to binary stream."
  (map 'nil #'(lambda (v) (write-integer v stream :bytes bytes :endianness endianness))
       seq))

(defun open-binary-stream (filespec &key (if-exists :overwrite) (if-does-not-exist :error))
  "Opens and returns binary stream. If filespec is a string or
pathname, the designed file is opened. If filespec is stream, filespec is returned."
  (etypecase filespec
    ((or string pathname)
     (cl:open filespec :direction :io :element-type 'unsigned-byte :if-exists if-exists :if-does-not-exist if-does-not-exist))
    (stream 
     filespec)))

(defmacro with-output-to-binary-array ((stream &optional array-form) &body body)
  "Returns byte array after executing the body from binary array stream."
  `(let ((,stream (make-binary-array-io-stream ,array-form)))
     ,@body
     (binary-array ,stream)))

(defmacro with-input-from-binary-array ((var array &key (index 0 index-p) start end) &body body)
  "Binds var to binary array input stream with given initial contents
and executes body with var available for reading."
  (declare (ignore start end))
  `(let ((,var (make-binary-array-input-stream ,array)))
     ,@(if index-p
          `((file-position ,var ,index)
            ,@body)
          body)))
