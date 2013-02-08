;;;; iso4217-codes.lisp

(in-package #:iso4217-codes)

(declaim (optimize (debug 3) (safety 3) (speed 0)))
;;; "iso4217-codes" goes here. Hacks and glory await!

(defparameter *browser* (make-instance 'browser))
(setf cl-mechanize:*user-agent* "Mozilla/5.0 (X11; Linux i686; rv:10.0.1) Gecko/20100101 Firefox/10.0.1")
(defparameter *wiki-table-url* "http://en.wikipedia.org/wiki/ISO_4217")

(defun strip-char-from-string (str char)
  "Return string with all instances of char removed."
  (concatenate 'string
               (loop for c across str when (not (equal c char)) collecting c)))

(defun strip-no-break-space (string)
  (strip-char-from-string string (code-char 160)))

(defun strip-string-garbage (string)
  "strip no-break_space from a string or list of strings."
  (cond
    ((listp string) (mapcar #'strip-no-break-space string))
    (t (strip-no-break-space string))))

(defun get-table-fields-in-row (tr)
  (let (inner-collector)
    (stp:do-recursively (b tr)
      (when (and (typep b 'stp:element)
                 (equal (stp:local-name b) "td"))
        (push (stp:string-value b) inner-collector)))
    (nreverse (strip-string-garbage inner-collector))))

(defun get-currency-codes ()
  (let ((getsite *wiki-table-url*))
    ;; (format t "~&~A" getsite)
    (fetch getsite *browser*)
    (let* ((page (browser-page *browser*)) ;; mechanize returns an object containing an stp dom
           (result (page-dom page))
           (col nil))
      ;; (format t "~A" page)
      (stp:do-recursively (a result)
          ;; (format t "~&~A" (stp:string-value a))
          (when (and (typep a 'stp:element)
                     (equal (stp:local-name a) "tr"))
            (push (get-table-fields-in-row a) col)))
      col)))

(defclass iso-currency-code ()
  ((iso-code :initarg :iso-code :initform nil :accessor iso-code)
   (description :initarg :description :initform nil :accessor description)
   (used-by :initarg :used-by :initform nil :accessor used-by)
   (replaced-by :initarg :replaced-by :initform nil :accessor replaced-by)))

(defun make-curr (vals)
  (cond ((= (length vals) 5)
         (make-instance 'iso-currency-code
                        :iso-code (nth 0 vals)
                        :description (nth 3 vals)))
        ((= (length vals) 7)
         (make-instance 'iso-currency-code
                        :iso-code (nth 0 vals)
                        :description (nth 3 vals)
                        :replaced-by (nth 4 vals)))))

(defun gather-all-monies (lists)
  (let ((kib lists))
    (loop for list in kib
          for a from 1
          :do (format t "~&[#~D][length: ~d] ~{ ~S ~}" a (length list) list)
          :when (make-curr list)
            :collecting it)))

(defun write-iso-file (filename)
  "write out a colon delimited file of iso4217 currency codes for use
in one of the info plugins of the DeepSky irc bot."
  (let ((monies (gather-all-monies (get-currency-codes))))
    (with-open-file (s filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop for money in monies
            :do (format s "~&~A:~A" (string-downcase (iso-code money)) (description money))))))
