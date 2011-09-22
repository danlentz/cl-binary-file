(in-package :common-lisp-user)

(require :unit-test)

(defpackage :org.alien-consader.system.file-formats.binary-file.little-endian
  (:use :asdf :cl)
  (:export #:little-endian))

(in-package :org.alien-consader.system.file-formats.binary-file.little-endian)

(defsystem little-endian
  :name "Alien consader's binary file little endian format support."
  :description "Binary file little endian format provides interface to read and write binary files."
  :version "0.1"
  :author "Sami Makinen <sami.o.makinen@gmail.com>"
  :components ((:file "package")
               (:file "io" :depends-on ("package")))
  :depends-on (:unit-test :binary-file))
