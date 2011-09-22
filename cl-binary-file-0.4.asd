(in-package :common-lisp-user)
(defpackage :system.binary-file
  (:use :asdf :cl)
  (:export #:cl-binary-file-0.4))

(in-package :system.binary-file)

(defsystem cl-binary-file-0.4
  :name "Binary file format support."
  :description "Binary file format provides interface to read and write binary files."
  :version "0.4"
  :author "Sami Makinen <sami.o.makinen@gmail.com>"
  :components ((:file "package")
               (:file "binary-file" :depends-on ("package"))
               (:module "little-endian" :depends-on ("binary-file" "vendor")
                        :components ((:file "package")
                                     (:file "io" :depends-on ("package"))
                                     (:module "test" :depends-on ("io")
                                              :components ((:module "unit"
                                                                    :components ((:file "io")))))))
               (:module "big-endian" :depends-on ("binary-file" "vendor")
                        :components ((:file "package")
                                     (:file "io" :depends-on ("package"))
                                     (:module "test" :depends-on ("io")
                                              :components ((:module "unit"
                                                                    :components ((:file "io")))))))
               (:module "vendor" 
                        :components ((:file "lisp-unit")))
               (:module "test" :depends-on ("binary-file" "vendor")
                        :components 
                        ((:module "unit"
                                  :components ((:file "binary-file"))))))
  :depends-on (:trivial-gray-streams))
