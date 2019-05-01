;;;; iso4217-codes.asd

(asdf:defsystem #:iso4217-codes
  :serial t
  :description "a package to coalesce iso4217 currency codes from the internet."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "MIT share alike."
  :depends-on (#:cl-mechanize
               #:cl-ppcre
               #:drakma
               #:str)
  :components ((:file "package")
               (:file "iso4217-codes")))

