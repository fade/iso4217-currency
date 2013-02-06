;;;; iso4217-codes.asd

(asdf:defsystem #:iso4217-codes
  :serial t
  :description "Describe iso4217-codes here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-mechanize
               #:cl-ppcre
               #:drakma)
  :components ((:file "package")
               (:file "iso4217-codes")))

