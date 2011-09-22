(in-package :common-lisp-user)

(require :unit-test)

(defpackage :org.alien-consader.system.file-formats.binary-file.big-endian
  (:use :asdf :cl)
  (:export #:big-endian))

(in-package :org.alien-consader.system.file-formats.binary-file.big-endian)

(defsystem big-endian
  :name "Alien consader's binary file big endian format support."
  :description "Binary file big endian format provides interface to read and write binary files."
  :version "0.1"
  :author "Sami Makinen <sami.o.makinen@gmail.com>"
  :components ((:file "package")
               (:file "io" :depends-on ("package")))
  :depends-on (:unit-test :binary-file))
