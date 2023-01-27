(defpackage #:com.inuoe.jzon
  (:use #:cl)
  (:export
   ;;; Read
   #:parse
   ;;;; Incremental reader
   #:make-parser
   #:parse-next
   #:close-parser
   #:with-parser

   ;;; Write
   #:stringify

   ;;; Types
   #:json-atom
   #:json-element

   ;;; Conditions
   #:json-error
   #:json-parse-error
   #:json-eof-error
   #:json-write-error
   #:json-recursive-write-error

   ;;; Simple extensible writing
   #:coerced-fields
   #:coerce-key

   ;;; Streaming Writer
   #:json-writer
   #:make-json-writer

   ;; Extensible serialization
   #:write-value

   #:begin-array
   #:write-values
   #:end-array
   #:with-array
   #:write-array

   ;; Writer operations
   #:begin-object
   #:write-key
   #:write-properties
   #:end-object
   #:with-object
   #:write-object

   ;; Dynavar interface to the writer
   #:*writer*
   #:with-writer*
   #:write-value*
   #:begin-array*
   #:write-values*
   #:end-array*
   #:with-array*
   #:write-array*
   #:begin-object*
   #:write-key*
   #:write-property*
   #:write-properties*
   #:end-object*
   #:with-object*
   #:write-object*)
  (:import-from #:closer-mop)
  (:import-from #:flexi-streams)
  (:import-from #:uiop))

(in-package #:com.inuoe.jzon)

(define-condition json-error (simple-error) ()
  (:documentation "Common error condition for all errors relating to reading/writing JSON."))

(define-condition json-parse-error (json-error)
  ((%line :initarg :line
          :reader %json-parse-error-line)
   (%column :initarg :column
            :reader %json-parse-error-column))
  (:report
   (lambda (c stream)
     (apply #'format stream (simple-condition-format-control c) (simple-condition-format-arguments c))
     (let ((line (%json-parse-error-line c))
           (column (%json-parse-error-column c)))
       (if (and line column)
           (format stream ", at line ~A, column ~A." (%json-parse-error-line c) (%json-parse-error-column c))
           (format stream ", position unavailable.")))))
  (:documentation "Error occuring while parsing JSON, in some cases with row/col information."))

(define-condition json-eof-error (json-parse-error) ()
  (:documentation "Error signalled when reaching the end of file while parsing JSON."))

(deftype json-atom ()
  "A 'native' atomic JSON value one can receive from `parse'.
  t => true
  nil => false
  null => null
  real => number"
  `(or (eql t)
       (eql nil)
       (eql null)
       real
       string))

(deftype json-element ()
  "A 'native' JSON value one can receive from `parse'.

  vector => list
  hash-table => object

see `json-atom'"
  `(or json-atom
       vector
       hash-table))

(declaim (type function *%pos-fn*))
(defvar *%pos-fn*)

(declaim (inline %ensure-function))
(defun %ensure-function (x)
  (if (functionp x) x (fdefinition x)))

(declaim (inline %raise))
(defun %raise (type format &rest args)
  (if (subtypep type 'json-parse-error)
      (multiple-value-bind (line column) (funcall *%pos-fn*)
        (error type :format-control format :format-arguments args :line line :column column))
      (error type :format-control format :format-arguments args)))

(declaim (inline %step))
(defun %step (step)
  (declare (function step))
  (values (the (or null character) (funcall step))))

(declaim (inline %read-string))
(defun %read-string (read-string)
  (declare (function read-string))
  (values (the simple-string (funcall read-string))))

(declaim (inline %key-fn))
(defun %key-fn (key-fn str)
  (declare (function key-fn))
  (declare (simple-string str))
  (values (the t (funcall key-fn str))))

(declaim (inline %whitespace-p))
(defun %whitespace-p (char)
  (and (member char '(#\Space #\Linefeed #\Return #\Tab))
       t))

(defun %skip-whitespace (step c allow-comments)
  "Skip whitespace, and optionally comments, depending on `%*allow-comments*'
 Returns the next character."
  (flet ((skip-cpp-comment ()
           ;; Skip the second slash, or open a block comment
           ;; NOTE - We intentionally follow C/C++ behaviour when it comes
           ;;        to disallowing nesting of /**/ style comments.
           (let ((c (%step step)))
             (case c
               ((nil) (%raise 'json-eof-error "End of input reading comment exepecting second slash or asterisk."))
               (#\/
                ;; Skip until LF or EOF
                (loop :until (member (%step step) '(nil #\Linefeed))))
               (#\*
                 ;; Skip until */ or error on EOF
                 (prog ((c (%step step)))
                   :expect-*
                   (case c
                     ((nil) (%raise 'json-eof-error "End of input reading block comment. Expecting '*/'."))
                     (#\*   (go :expect-/))
                     (t     (setf c (%step step))
                            (go :expect-*)))
                   :expect-/
                   (setf c (%step step))
                   (case c
                     ((nil) (%raise 'json-eof-error "End of input reading block comment. Expecting '/'."))
                     (#\/ nil) ; done
                     (t   (go :expect-*)))))
               (t (%raise 'json-parse-error "Unexpected input '/~A'. Expecting // or /* to begin comment." c))))))
    (loop :for char := c :then (%step step)
          :do (cond
                ((null char)
                 (return nil))
                ((and (char= #\/ char) allow-comments)
                 (skip-cpp-comment))
                ((not (%whitespace-p char))
                 (return char))))))

(defun %control-char-p (c)
  "Returns true if `c' is a control character per RFC 8259."
  (declare (type character c))
  (<= #x00 (char-code c) #x1F))

(defun %read-json-string (step string-accum max-string-length)
  "Reads a JSON string step-wise using `step' until an unescaped double-quote.
 Returns a `simple-string' representing the string."
  (declare (type (and string (not simple-string)) string-accum))
  (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
  (labels ((interpret (char)
             (cond
               ((char= #\\ char)
                (let ((escaped (%step step)))
                  (case escaped
                    ((nil) (%raise 'json-eof-error "Unexpected end of input after '\\' in string"))
                    ((#.(char "\"" 0) #\\ #\/) escaped)
                    (#\b #\Backspace)
                    (#\f #\Page)
                    (#\n #\Linefeed)
                    (#\r #\Return)
                    (#\t #\Tab)
                    (#\u (read-unicode))
                    (t (%raise 'json-parse-error "Invalid escape sequence in string '\\~A'" escaped)))))
               ((%control-char-p char)
                (%raise 'json-parse-error "Unexpected control character in string '~A' (~A)" char (char-name char)))
               (t char)))
           (read-unicode ()
             ;; refer to ECMA-404, strings.
             (flet ((read-code-point ()
                      (the (unsigned-byte 32)
                           (loop :for pos :of-type (integer 0 4) :from 0 :below 4
                                 :for weight :of-type (integer 1 4096) := #.(expt 16 3) :then (ash weight -4)
                                 :for digit := (digit-char-p (%step step) 16)
                                 :do (unless digit (%raise 'json-parse-error "Invalid unicode constant in string"))
                                 :sum (the (unsigned-byte 32) (* digit weight)) :of-type (unsigned-byte 32))))
                    (expect-char (char)
                      (let ((c (%step step)))
                        (unless (char= char c)
                          (%raise 'json-parse-error "Expecting ~C, found ~C instead" char c)))))
               (let ((code-point (read-code-point)))
                 (declare (type (unsigned-byte 32) code-point))
                 (code-char
                  (if (<= #xD800 code-point #xDBFF)
                      (let ((utf-16-high-surrogate-pair code-point))
                        (expect-char #\\)
                        (expect-char #\u)
                        (let ((utf-16-low-surrogate-pair (read-code-point)))
                          (declare (type (unsigned-byte 32) utf-16-low-surrogate-pair))
                          (unless (<= #xDC00 utf-16-low-surrogate-pair #xDFFF)
                            (%raise 'json-parse-error "Unexpected UTF-16 surrogate pair: ~A and ~A"
                                    utf-16-high-surrogate-pair
                                    utf-16-low-surrogate-pair))
                          (+ #x010000
                             (ash (- utf-16-high-surrogate-pair #xD800) 10)
                             (- utf-16-low-surrogate-pair #xDC00))))
                      code-point))))))
    (setf (fill-pointer string-accum) 0)
    (loop :for next :of-type character := (or (%step step) (%raise 'json-eof-error "Encountered end of input inside string constant"))
          :until (char= #.(char "\"" 0) next)
          :do
             (when (= (fill-pointer string-accum) max-string-length)
               (%raise 'json-parse-error "Maximum string length exceeded"))
             (vector-push-extend (interpret next) string-accum))
    (make-array (fill-pointer string-accum) :element-type (if (every (lambda (c) (typep c 'base-char)) (the (and string (not simple-string)) string-accum)) 'base-char 'character) :initial-contents string-accum)))

(defun %read-json-number (step c)
  "Reads an RFC 8259 number, starting with `c'."
  (flet ((digit09-p (c &aux (val (- (char-code c) (char-code #\0))))
           (and (<= 0 val 9) val))
         (digit19-p (c &aux (val (- (char-code c) (char-code #\0))))
           (and (<= 1 val 9) val))
         (ends-number-p (c)
           (or (%whitespace-p c) (find c "]},/"))))
    (macrolet ((takec (on-eof)
                 "Take the next character, `go'ing to `label' on EOF or end of token"
                 `(let ((c (%step step)))
                    (when (or (null c) (ends-number-p c))
                      (setf lc c)
                      (go ,on-eof))
                    c)))
      (prog ((int-sign 1)
             (int-val 0)
             (frac-val 0)
             (frac-len 0)
             (exp-sign 1)
             (exp-val 0)
             (lc c))
         (declare (type (integer 0) int-val frac-val exp-val frac-len)
                  (type (member -1 1) int-sign exp-sign))
         (let ((c c))
           (when (char= c #\-)
             (setf int-sign -1)
             (setf c (takec :fail)))

           (when (char= c #\0)
             (case (takec :done-0)
               (#\.       (go :parse-frac))
               ((#\e #\E) (go :parse-exp))
               (t         (go :fail))))

           (let ((digit (digit19-p c)))
             (unless digit (go :fail))
             (setf int-val digit)
             (go :parse-int)))

       :done-0
         (return (values (if (plusp int-sign) 0 -0.0d0) lc))

       :parse-int
         (let ((c (takec :done-int)))
           (case c
             (#\.       (go :parse-frac))
             ((#\e #\E) (go :parse-exp)))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf int-val (+ (* int-val 10) digit))))
         (go :parse-int)

       :done-int
         (return (values (* int-sign int-val) lc))

       :parse-frac
         (let* ((c (takec :fail))
                (digit (digit09-p c)))
           (unless digit (go :fail))
           (setf frac-val digit)
           (setf frac-len 1))

       :parse-frac-loop
         (let ((c (takec :done-frac)))
           (when (or (char= c #\e) (char= c #\E))
             (go :parse-exp))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf frac-val (+ (* frac-val 10) digit))
             (incf frac-len)))
         (go :parse-frac-loop)

       :done-frac
         (return (values (* int-sign (+ int-val (/ frac-val (expt 10.d0 frac-len)))) lc))

       :parse-exp
         (let ((c (takec :fail)))
           (case c
             (#\-
              (setf exp-sign -1)
              (setf c (takec :fail)))
             (#\+
              (setf c (takec :fail))))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf exp-val digit)))

       :parse-exp-loop
         (let* ((c (takec :done))
                (digit (digit09-p c)))
           (unless digit (go :fail))
           (setf exp-val (+ (* exp-val 10) digit)))
         (go :parse-exp-loop)

       :done
         (return
           (values
            (let ((exp-mult (expt 10d0 (* exp-sign exp-val))))
              (* int-sign
                (+ (* int-val exp-mult)
                   (/ (* frac-val exp-mult) (expt 10.d0 frac-len)))))
            lc))
       :fail
         (return (values nil lc))))))

(macrolet ((def-make-string-fns (name type)
             `(defun ,name (in max-string-length)
                "Create step, and read-string functions for the string `in'."
                (declare (type ,type in))
                (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
                (let ((i 0))
                  (declare (type (integer 0 #.array-dimension-limit) i))
                  (let* ((step (lambda () (when (< i (length in)) (prog1 (aref in i) (incf i)))))
                         (read-string (let ((string-accum (make-array (min 1024 array-dimension-limit) :element-type 'character :adjustable t :fill-pointer 0)))
                                        (lambda ()
                                          ;; Scan until we hit a closing "
                                          ;; Error on EOF
                                          ;; Error if we encounter a literal control char
                                          ;; Track suitable element-type
                                          (loop
                                            :with element-type := 'base-char
                                            :for j :from i
                                            :do
                                              (when (<= (length in) j)
                                                (%raise 'json-eof-error "Unexpected end of input when reading string."))
                                              (let ((c (aref in j)))
                                                (when (char= c #.(char "\"" 0))
                                                  (let ((len (- j i)))
                                                    (when (< max-string-length len)
                                                      (setf i (+ i (1+ max-string-length)))
                                                      (%raise 'json-parse-error "Maximum string length exceeded"))
                                                    (return
                                                      (loop :with ret := (make-array len :element-type element-type)
                                                            :for k :from 0 :below len
                                                            :do (setf (aref ret k) (aref in (+ i k)))
                                                            :finally
                                                            (setf i (1+ j))
                                                            (return ret)))))
                                                (when (char= c #\\) ;; we need to worry about escape sequences, unicode, etc.
                                                  (return (%read-json-string step string-accum max-string-length)))

                                                (when (<= #x00 (char-code c) #x1F)
                                                  (%raise 'json-parse-error "Unexpected control character in string '~A' (~A)" c (char-name c)))

                                                (when (not (typep c 'base-char))
                                                  (setf element-type 'character)))))))
                         (pos (lambda ()
                                (loop :with line := 1
                                      :with col := 1
                                      :with cr := nil
                                      :for p :from 0 :below (1- i)
                                      :for c := (aref in p)
                                      :do (case c
                                            (#\Linefeed (incf line) (setf col 1))
                                            (t (incf col)))
                                      :finally
                                         (return (values line col))))))
                    (values step
                            read-string
                            pos))))))
  (def-make-string-fns %make-fns-simple-string simple-string)
  (def-make-string-fns %make-fns-string (and string (not simple-string))))

(defun %make-fns-stream (in max-string-length)
  "Create step, and read-string functions for the stream `in'."
  (declare (type stream in))
  (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
  (unless (subtypep (stream-element-type in) 'character)
    (return-from %make-fns-stream (%make-fns-stream (flexi-streams:make-flexi-stream in :external-format :utf-8) max-string-length)))
  (let* ((step (lambda () (read-char in nil)))
         (read-string (let ((string-accum (make-array (min 1024 array-dimension-limit) :element-type 'character :adjustable t :fill-pointer 0)))
                        (lambda () (%read-json-string step string-accum max-string-length))))
         (pos (lambda ()
                (block nil
                  (let ((pos (ignore-errors (file-position in))))
                    (unless (and pos (ignore-errors (file-position in 0)))
                      (return (values nil nil)))

                    (loop :with line := 1
                          :with col := 1
                          :with cr := nil
                          :for p :from 0 :below (1- pos)
                          :for c := (read-char in)
                          :do (case c
                                (#\Linefeed (incf line) (setf col 1))
                                (t (incf col)))
                          :finally
                             (file-position in pos)
                             (return (values line col))))))))
    (values step
            read-string
            pos)))

(defclass parser ()
  ((%step
    :type function)
   (%read-string
    :type function)
   (%pos
    :type function)
   (%max-depth
    :initform nil
    :type (or null (integer 1)))
   (%allow-comments
    :initform nil
    :type boolean)
   (%allow-trailing-comma
    :initform nil
    :type boolean)
   (%key-fn
    :type function)
   (%max-string-length
    :initform (min #x100000 array-dimension-limit)
    :type (integer 1 (#.array-dimension-limit)))
   (%state
    :initform nil
    :type symbol)
   (%lookahead
    :initform nil
    :type (or null character))
   (%context
    :initform (list)
    :type list)
   (%encountered-top-value
    :initform nil
    :type boolean)
   (%depth
    :initform 0
    :type (integer 0))
   (%close-action
    :type (or null function)))
  (:documentation "An incremental JSON parser.

see `make-parser'
see `next'
see `close-parser'"))

(defun make-parser (in &key
                      (max-depth 128)
                      (allow-comments nil)
                      (allow-trailing-comma nil)
                      (max-string-length (min #x100000 array-dimension-limit))
                      key-fn)
  "Construct a `parser' Read a JSON value from `in', which may be a vector, a stream, or a pathname.
 `:max-depth' controls the maximum depth of the object/array nesting
 `:allow-comments' controls whether or not single-line // comments are allowed.
 `:allow-trailing-comma' controls whether or not a single comma `,' is allowed after all elements of an array or object.
 `:key-fn' is a function of one value which 'pools' object keys, or null for the default pool

see `next'
see `close-parser'"
  (check-type max-depth (or (integer 1) null))
  (check-type max-string-length (integer 1 (#.array-dimension-limit)))
  (check-type key-fn (or null symbol function))

  (multiple-value-bind (input close-action)
      (typecase in
        (pathname
          (let ((f (open in :direction :input :external-format :utf-8)))
            (values f (lambda () (close f)))))
        ((vector (unsigned-byte 8))
          (let* ((bstream (flexi-streams:make-in-memory-input-stream in))
                 (fstream (flexi-streams:make-flexi-stream bstream :external-format :utf-8)))
            (values fstream (lambda ()
                              (close fstream)
                              (close bstream)))))
        (t (values in nil)))
    (let ((parser (make-instance 'parser)))
      (with-slots (%step %read-string %pos %max-depth %allow-comments %allow-trailing-comma %max-string-length %key-fn %close-action) parser
        (setf %close-action close-action)
        (setf (values %step %read-string %pos)
              (etypecase input
                (simple-string (%make-fns-simple-string input max-string-length))
                (string (%make-fns-string input max-string-length))
                (stream (%make-fns-stream input max-string-length))))

        (setf %max-depth max-depth)
        (setf %allow-comments (and allow-comments t))
        (setf %allow-trailing-comma (and allow-trailing-comma t))
        (setf %key-fn (etypecase key-fn
                        (null (%make-string-pool))
                        (function key-fn)
                        (symbol (let ((sym key-fn))
                                  (lambda (str)
                                    (funcall sym str)))))))
      parser)))

(defun close-parser (parser)
  "Close the `parser'"
  (check-type parser parser)
  (let ((action (shiftf (slot-value parser '%close-action) nil)))
    (when action
      (funcall action)
      (slot-makunbound parser '%step)
      (slot-makunbound parser '%read-string)
      (slot-makunbound parser '%pos)
      (slot-makunbound parser '%key-fn)))
  parser)

(defmacro with-parser ((name &rest args) &body body)
  `(let ((,name (make-parser ,@args)))
    (unwind-protect (locally ,@body)
      (close-parser ,name))))

(defun %parse-next (parser %step %read-string %key-fn %max-depth %allow-trailing-comma %allow-comments)
  (declare (type parser parser)
           (type function %step %read-string %key-fn)
           (type (or null (integer 1)) %max-depth)
           (type boolean %allow-trailing-comma)
           (type boolean %allow-comments))
  (with-slots (%state %context %lookahead %depth %encountered-top-value) parser
    (labels ((read-element (lc)
              (declare (type character lc))
              (macrolet ((expect (string value)
                           `(progn
                              ,@(loop :for i :from 1 :below (length string)
                                      :for expect-c := (char string i)
                                      :collect `(let ((c (or (%step %step)
                                                             (%raise 'json-eof-error (format nil "End of input when reading token '~A' - expected '~A'" ,string ,expect-c)))))
                                                  (unless (char= c ,expect-c)
                                                    (%raise 'json-parse-error (format nil "Unexpected token '~A'" (concatenate 'string
                                                                                                                   ,(subseq string 0 i)
                                                                                                                   (loop :for c := c :then (%step %step)
                                                                                                                         :until (or (null c) (%whitespace-p c) (find c "{}[],-/"))
                                                                                                                         :collect c)))))))
                              (setf %state (car %context))
                              (values :value ,value))))
                (case lc
                  (#\[              (push-state :begin-array))
                  (#\{              (push-state :begin-object))
                  (#.(char "\"" 0)  (setf %state (car %context))
                                    (values :value (%read-string %read-string)))
                  (#\f              (expect "false" nil))
                  (#\t              (expect "true" t))
                  (#\n              (expect "null" 'null))
                  (t                (multiple-value-bind (number lookahead)
                                        (%read-json-number %step lc)
                                      (unless number
                                        (if lookahead
                                          (%raise 'json-parse-error "Unexpected character in JSON data '~C' (~A)" lookahead (char-name lookahead))
                                          (%raise 'json-parse-error "End of input when reading number")))
  
                                      (setf %lookahead lookahead)
                                      (setf %state (car %context))
                                      (values :value number))))))
              (push-state (kind)
                (incf (the fixnum %depth))
                (when (and %max-depth (> (the fixnum %depth) (the fixnum %max-depth)))
                  (%raise 'json-parse-error "Maximum depth exceeded"))
                (setf %state kind)
                (values kind nil))
              (pop-state (kind)
                (decf (the fixnum %depth))
                (pop %context)
                (setf %state (car %context))
                (values kind nil)))
      (declare (dynamic-extent #'read-element #'push-state #'pop-state))
      (ecase %state
        ((nil)
          (let ((lc (%skip-whitespace %step (or (shiftf %lookahead nil) (%step %step)) %allow-comments)))
            (cond
              ((and %encountered-top-value lc)
                (setf %lookahead lc)
                (%raise 'json-parse-error "Content after reading element"))
              (%encountered-top-value
                (values nil nil nil))
              ((null lc)
                (%raise 'json-eof-error "End of input when reading JSON element"))
              (t
                (setf %encountered-top-value t)
                (read-element lc)))))
        (:begin-array
          (let ((lc (%skip-whitespace %step (%step %step) %allow-comments)))
            (case lc
              ((nil)  (%raise 'json-parse-error "End of input when reading array, expecting element or array close"))
              (#\]    (decf (the fixnum %depth))
                      (setf %state (car %context))
                      (values :end-array nil))
              (t      (push 'after-read-array-element %context)
                      (read-element lc)))))
        (after-read-array-element
          (let ((lc (%skip-whitespace %step (or (shiftf %lookahead nil) (%step %step)) %allow-comments)))
            (case lc
              ((nil)  (%raise 'json-parse-error "End of input when reading array, expecting comma or array close"))
              (#\,    (let ((lc (%skip-whitespace %step (%step %step) %allow-comments)))
                        (case lc
                          ((nil)  (if %allow-trailing-comma
                                    (%raise 'json-parse-error "End of input when reading array, expecting element or array close")
                                    (%raise 'json-parse-error "End of input when reading array, expecting element")))
                            (#\]  (unless %allow-trailing-comma
                                    (%raise 'json-parse-error "Trailing comma when reading array"))
                                  (pop-state :end-array))
                            (t    (read-element lc)))))
              (#\]    (pop-state :end-array))
              (t      (%raise 'json-parse-error "Unexpected character '~A' (~A) when reading array, expecting comma or array close" lc (char-name lc))))))
        (:begin-object
          (let ((lc (%skip-whitespace %step (%step %step) %allow-comments)))
            (case lc
              ((nil)            (%raise 'json-parse-error "End of input when reading object, expecting key or object close"))
              (#\}              (decf (the fixnum %depth))
                                (setf %state (car %context))
                                (values :end-object nil))
              (#.(char "\"" 0)  (setf %state 'after-read-key)
                                (let ((key (%key-fn %key-fn (%read-string %read-string))))
                                  (push 'after-read-property %context)
                                  (values :object-key key nil)))
              (t                (%raise 'json-parse-error "Unexpected character '~A' (~A) when reading object, expecting key" lc (char-name lc))))))
        (after-read-key
          (let ((lc (%skip-whitespace %step (%step %step) %allow-comments)))
            (case lc
              ((nil) (%raise 'json-parse-error "End of input when reading object, expecting colon after object key"))
              (#\:)
              (t     (%raise 'json-parse-error "Unexpected character '~A' (~A) when reading object, expecting colon after object key" lc (char-name lc)))))

          (let ((lc (%skip-whitespace %step (%step %step) %allow-comments)))
            (case lc
              ((nil)  (%raise 'json-parse-error "End of input when reading object, expecting value after colon"))
              (t      (read-element lc)))))
        (after-read-property
          (let ((lc (%skip-whitespace %step (or (shiftf %lookahead nil) (%step %step)) %allow-comments)))
            (case lc
              ((nil)  (%raise 'json-parse-error "End of input when reading object. Expecting comma or object close"))
              (#\,    (let ((lc (%skip-whitespace %step (%step %step) %allow-comments)))
                        (case lc
                          ((nil)
                            (if %allow-trailing-comma
                              (%raise 'json-parse-error "End of input when reading object. expected key or object close after comma")
                              (%raise 'json-parse-error "End of input when reading object, expected key after comma")))
                          (#\}
                            (unless %allow-trailing-comma
                              (%raise 'json-parse-error "Trailing comma when reading object"))
                            (pop-state :end-object))
                          (#.(char "\"" 0)
                            (setf %state 'after-read-key)
                            (let ((key (%key-fn %key-fn (%read-string %read-string))))
                              (values :object-key key nil)))
                          (t
                            (if %allow-trailing-comma
                              (%raise 'json-parse-error "Unexpected character '~A' (~A) when reading object, expecting key or object close after comma" lc (char-name lc))
                              (%raise 'json-parse-error "Unexpected character '~A' (~A) when reading object, expecting key after comma" lc (char-name lc)))))))
              (#\}    (pop-state :end-object))
              (t      (%raise 'json-parse-error "Unexpected character '~A' (~A) when reading object, expecting comma or object close" lc (char-name lc))))))))))

(defun parse-next (parser)
  "Read the next token from `parser'. Depending on the token, returns 1 or 2 values:

  :value, value       ; A `json-atom' <value>
  :begin-array, nil   ; Array open [
  :end-array, nil     ; Array close ]
  :begin-object, nil  ; Object open {
  :object-key, key    ; Object key
  :end-object, nil    ; Object finished }

see `make-parser'
see `close-parser'"
  (check-type parser parser)
  (let ((*%pos-fn* (slot-value parser '%pos)))
    (with-slots (%step %read-string %key-fn %max-depth %allow-trailing-comma %allow-comments) parser
      (%parse-next parser %step %read-string %key-fn %max-depth %allow-trailing-comma %allow-comments))))

(defun %make-string-pool ()
  "Make a function for 'interning' strings in a pool."
  (let ((pool (make-hash-table :test 'equal)))
    (lambda (key)
      (declare (type simple-string key))
      (etypecase pool
        (list
          (loop :for elt :of-type simple-string :in pool
                :for i :from 0
                :do
                  (when (string= key elt)
                    (return elt))
                  (when (> i 64) ;; had to search too long
                    (let ((old pool)
                          (new (make-hash-table :size (* i 2) :test 'equal)))
                     (dolist (key old)
                       (setf (gethash key new) key))
                     (setf pool new))

                     (return (or (gethash key pool)
                                 (setf (gethash key pool) key))))
                :finally (push key pool)
                         (return key)))
        (hash-table
          (or (gethash key pool)
              (setf (gethash key pool) key)))))))

(defun parse (in &key
                   (max-depth 128)
                   (allow-comments nil)
                   (allow-trailing-comma nil)
                   (max-string-length (min #x100000 array-dimension-limit))
                   key-fn)
  "Read a JSON value from `in', which may be a vector, a stream, or a pathname.
 `:max-depth' controls the maximum depth of the object/array nesting
 `:allow-comments' controls whether or not single-line // comments are allowed.
 `:allow-trailing-comma' controls whether or not a single comma `,' is allowed after all elements of an array or object.
 `:key-fn' is a function of one value which 'pools' object keys, or null for the default pool"
  (check-type max-depth (or (integer 1) null))
  (check-type key-fn (or null symbol function))
  (check-type max-string-length (integer 1 (#.array-dimension-limit)))
  (typecase in
    (pathname
     (with-open-file (in in :direction :input :external-format :utf-8)
       (parse in :max-depth max-depth :allow-comments allow-comments :key-fn key-fn)))
    ((vector (unsigned-byte 8))
     (flexi-streams:with-input-from-sequence (stream in)
       (let ((stream (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
         (parse stream :max-depth max-depth :allow-comments allow-comments :key-fn key-fn))))
    (t
      (with-parser (parser in
                          :max-depth max-depth :allow-comments allow-comments
                          :allow-trailing-comma allow-trailing-comma
                          :max-string-length max-string-length
                          :key-fn (or (and key-fn (%ensure-function key-fn))
                                      (%make-string-pool)))

        (let (top stack key)
          (declare (dynamic-extent stack key))
          (flet ((finish-value (value)
                    (typecase stack
                      (null
                        (setf top value))
                      ((cons list)
                        (push value (the list (car stack))))
                      ((cons hash-table)
                        (setf (gethash (pop key) (the hash-table (car stack))) value)))))
              (declare (dynamic-extent #'finish-value))
              (let ((*%pos-fn* (slot-value parser '%pos))
                    (%step (slot-value parser '%step))
                    (%read-string (slot-value parser '%read-string))
                    (%key-fn (slot-value parser '%key-fn))
                    (%max-depth (slot-value parser '%max-depth))
                    (%allow-trailing-comma (slot-value parser '%allow-trailing-comma))
                    (%allow-comments (slot-value parser '%allow-comments)))
                (loop
                  (multiple-value-bind (evt value) (%parse-next parser %step %read-string %key-fn %max-depth %allow-trailing-comma %allow-comments)
                    (ecase evt
                      ((nil)          (return top))
                      (:value         (finish-value value))
                      (:begin-array   (push (list) stack))
                      (:end-array     (finish-value (coerce (the list (nreverse (pop stack))) 'simple-vector)))
                      (:begin-object  (push (make-hash-table :test 'equal) stack))
                      (:object-key    (push value key))
                      (:end-object    (finish-value (pop stack)))))))))))))

(macrolet ((%coerced-fields-slots (element)
             `(let ((class (class-of ,element)))
                (c2mop:ensure-finalized class)
                (mapcar (lambda (s)
                          (list (c2mop:slot-definition-name s)
                                (c2mop:slot-value-using-class class ,element s)
                                (c2mop:slot-definition-type s)))
                        (remove-if-not (lambda (s) (c2mop:slot-boundp-using-class class ,element s))
                                       (c2mop:class-slots class))))))
  (defgeneric coerced-fields (element)
    (:documentation "Return a list of key definitions for `element'.
 A key definition is a three-element list of the form
  (name value &optional type)
 name is the key name and will be coerced if not already a string
 value is the value, and will be written per `write-value'
 type is a type for the key, in order to handle ambiguous `nil' interpretations.

Example return value:
  ((name :zulu)
   (hobbies nil list))
")
    (:method (element)
      nil)
    #+(or ccl clisp sbcl)
    (:method ((element structure-object))
      (%coerced-fields-slots element))
    (:method ((element standard-object))
      (%coerced-fields-slots element))))

(defgeneric coerce-key (key)
  (:documentation "Coerce `key' into a string designator, or `nil' if `key' is an unsuitable key.")
  (:method (key)
    (format nil "~A" key))
  (:method ((key symbol))
    (let ((name (symbol-name key)))
      (if (some #'lower-case-p name)
          name
          (string-downcase name))))
  (:method ((key string))
    key)
  (:method ((key character))
    (string key))
  (:method ((key integer))
    (format nil "~D" key)))

(define-condition json-write-error (json-error) ()
  (:documentation "Error signalled when there is an issue during writing JSON."))

(define-condition json-recursive-write-error (json-write-error)
  ((%path :initarg :path
          :reader %json-recursive-write-error-path))
  (:report
   (lambda (c stream)
     (apply #'format stream (simple-condition-format-control c) (simple-condition-format-arguments c))
     (let ((*print-circle* t))
       (format stream ", path until recursion point: ~A" (%json-recursive-write-error-path c)))))
  (:documentation "Error signalled when a recursive write is detected.
  A recursive write is when `write-value' is called from within `write-value' with a value it has 'seen' before."))

;;; Stack is
;;; (<state> . <prev-states>)
;;; where <state> is one of
;;;   - :object
;;;   - :object-key
;;;   - :object-value
;;;   - :array
;;;   - :array-value
;;;   - :complete
;;; :object means we are at the beginning of an object
;;;   so we expect a KEY OR object close
;;; :object-key means we have just written a KEY,
;;;   so we expect a VALUE
;;; :object-value means we have just written a value
;;;   so we expect a KEY OR object close
;;; :array means we have BEGUN writing an array, and expect
;;;   a VALUE OR array close
;;; :array-value means we have written an array value, and expect
;;;   a VALUE OR array close
;;; :complete signals we have finished writing a toplevel value

(defclass json-writer ()
  ((%stream :initarg :stream)
   (%coerce-key :initarg :coerce-key)
   (%pretty :initarg :pretty)
   (%stack :initform nil)
   (%ref-stack :initform nil)
   (%replacer :initarg :replacer)
   (%depth :type integer :initform 0)
   (%max-depth :initarg :max-depth))
  (:documentation "A JSON writer on which to call `write-value', `begin-object', etc.")
  (:default-initargs
   :stream (make-broadcast-stream)
   :coerce-key #'coerce-key
   :pretty nil
   :replacer nil
   :max-depth 128))

(defun make-json-writer (&key
                           (stream (make-broadcast-stream))
                           (coerce-key #'coerce-key)
                           (pretty nil)
                           (replacer nil)
                           (max-depth 128))
  "Create a writer for subsequent `write-value', `begin-object', et al calls.
  `:stream' must be a character or binary `stream'
  `:replacer' a function of two arguments, the key and the value of a KV pair.
  `:coerce-key' is a function of one argument, and is used to coerce object keys into non-nil string designators

 see `coerce-key'"
  (check-type stream stream)
  (check-type max-depth (or null (integer 1)))
  (let ((stream (cond
                  ((subtypep (stream-element-type stream) 'character) stream)
                  (t (flexi-streams:make-flexi-stream stream :external-format :utf-8)))))
    (make-instance 'json-writer :stream stream
                                :coerce-key (%ensure-function coerce-key)
                                :pretty (and pretty t)
                                :replacer replacer
                                :max-depth max-depth)))

(defun %write-indentation (writer)
  "Indent `writer' depending on its depth, if it is set to pretty print."
  (with-slots (%stream %stack %pretty) writer
    (when %pretty
      (write-char #\NewLine %stream)
      (loop :for _ :in %stack
            :do
               (write-char #\Space %stream)
               (write-char #\Space %stream)))))

(defun %write-json-string (string stream)
  "Write `string' to `stream' as a JSON string."
  (write-char #.(char "\"" 0) stream)
  (loop :for c :across string
        :do
           (case c
             ((#.(char "\"" 0) #\\)
              (write-char #\\ stream)
              (write-char c stream))
             (#\Backspace
              (write-char #\\ stream)
              (write-char #\b stream))
             (#\Page
              (write-char #\\ stream)
              (write-char #\f stream))
             (#\Linefeed
              (write-char #\\ stream)
              (write-char #\n stream))
             (#\Return
              (write-char #\\ stream)
              (write-char #\r stream))
             (#\Tab
              (write-char #\\ stream)
              (write-char #\t stream))
             (t
              (cond
                ((%control-char-p c)
                 (format stream "\\u~4,'0X" (char-code c)))
                (t
                 (write-char c stream))))))
  (write-char #.(char "\"" 0) stream))

(defun %write-atom-value (writer value)
  "Write a JSON atom per `json-atom'."
  (with-slots (%stream %stack) writer
    (case (car %stack)
      ((:array :object)
       (%write-indentation writer))
      ((:array-value)
       (write-char #\, %stream)
       (%write-indentation writer)))

    (etypecase value
      ((eql t)      (write-string "true" %stream))
      ((eql nil)    (write-string "false" %stream))
      ((eql null)   (write-string "null" %stream))
      (integer      (format %stream "~D" value))
      ;; TODO - Double-check any edge-cases with ~F and if ~E might be more appropriate
      (real         (format %stream "~F" value))
      (string       (%write-json-string value %stream)))))

(defun begin-object (writer)
  "Begin writing an object to `writer'.

see `write-key'
see `write-property'
see `write-properties'
see `end-object'"
  (check-type writer json-writer)
  (with-slots (%stream %stack %depth %max-depth) writer
    (case (car %stack)
      ((:array)                     (progn (%write-indentation writer)
                                           (setf (car %stack) :array-value)))
      ((:array-value)               (progn (write-char #\, %stream)
                                           (%write-indentation writer)))
      ((:object-key)                (setf (car %stack) :object-value))
      ((:object :object-value)      (error "Expecting object key"))
      ((:complete)                  (error "Attempting to write object when value already written to json-writer")))
    (when (and %max-depth (> (incf %depth) %max-depth))
      (error "Exceeded maximum depth in writing object."))
    (push :object %stack)
    (write-char #\{ %stream))
  writer)

(defun write-key (writer key)
  "Write an object `key' to `writer'. Must currently be writing an object.

see `begin-object'
see `with-object'"
  (check-type writer json-writer)
  (with-slots (%stream %coerce-key %stack %pretty) writer
    (let ((context (car %stack)))
      (case context
        (:object       (progn (setf (car %stack) :object-key)
                              (%write-indentation writer)))
        (:object-value (progn (write-char #\, %stream)
                              (%write-indentation writer)
                              (setf (car %stack) :object-key)))
        (t             (error "Attempting to write object key while in ~A" context))))
    (let ((key-str (funcall %coerce-key key)))
      (unless (typep key-str '(or string character (and (not null) symbol)))
        (error "Invalid key after coercion: '~A' -> '~A'" key key-str))
      (%write-json-string (string key-str) %stream))
    (write-char #\: %stream)
    (when %pretty
      (write-char #\Space %stream)))
  writer)

(defun end-object (writer)
  "Finish writing an object to `writer'. Must match an opening `begin-object'."
  (check-type writer json-writer)
  (with-slots (%stream %stack %pretty %depth %max-depth) writer
    (let ((context (car %stack)))
      (case context
        ((:object :object-value))
        (:object-key (error "Attempting to close object before writing key value"))
        (t           (error "Attempting to close object while in ~A" context)))
      (pop %stack)
      (when %max-depth
        (decf %depth))
      (when (eq context :object-value)
        (%write-indentation writer)))
    (write-char #\} %stream)

    (unless %stack
      (push :complete %stack)))
  writer)

(defmacro with-object (writer &body body)
  "Wrapper around `begin-object' and `end-object'.

see `write-object'
see `write-property'
see `write-properties'"
  (let ((writer-sym (gensym "WRITER")))
    `(let ((,writer-sym ,writer))
       (begin-object ,writer-sym)
       ,@body
       (end-object ,writer-sym))))

(defun begin-array (writer)
  "Begin writing an array to `writer'.

see `write-values'
see `end-array'"
  (check-type writer json-writer)
  (with-slots (%stream %stack %depth %max-depth) writer
    (case (car %stack)
      ((:array-value)          (progn
                                 (write-char #\, %stream)
                                 (%write-indentation writer)))
      ((:object-key)           (setf (car %stack) :object-value))
      ((:object :object-value) (error "Expecting object key"))
      ((:complete)             (error "Attempting to write array when value already written to json-writer")))
    (push :array %stack)
    (when (and %max-depth (> (incf %depth) %max-depth))
      (error "Exceeded maximum depth in writing array."))
    (write-char #\[ %stream))
  writer)

(defun end-array (writer)
  "Finish writing an array to `writer'. Must match an opening `begin-array'."
  (check-type writer json-writer)
  (with-slots (%stream %stack %depth %max-depth) writer
    (let ((context (car %stack)))
      (case context
        ((:array :array-value))
        (t (error "Attempting to close array while in ~A" context)))
      (pop %stack)
      (when %max-depth
        (decf %depth))
      (when (eq context :array-value)
        (%write-indentation writer)))
    (write-char #\] %stream)

    (unless %stack
      (push :complete %stack)))
  writer)

(defmacro with-array (writer &body body)
  "Wrapper around `begin-array' and `end-array'

see `write-array'
see `write-values'"
  (let ((writer-sym (gensym "WRITER")))
    `(let ((,writer-sym ,writer))
       (begin-array ,writer-sym)
       (progn ,@body)
       (end-array ,writer-sym))))

(defgeneric write-value (writer value)
  (:documentation "Write a JSON value to `writer'. Specialize this function for customized JSON writing.")
  (:method :around ((writer json-writer) value)
    (with-slots (%stack %ref-stack %pretty %replacer) writer
      (let ((context (car %stack)))
        (case context
          ((:object :object-value) (error "Expecting object key"))
          ((:complete)             (error "Attempting to write value when value already written to json-writer")))

        (let ((prev-stack %ref-stack))
          (let ((path (member value prev-stack :test #'eq)))
            (when path
              ;; bail with a ref string
              (error 'json-recursive-write-error :format-control "Recursion detected printing value" :path (reverse path))))
          (setf %ref-stack (cons value prev-stack))

          ;; Call the replacer on the top-level object first, if applicable

          (unwind-protect (progn
                            (call-next-method writer (if (and (null context) %replacer)
                                                       (multiple-value-call
                                                           (lambda (write-p &optional (new-value nil value-changed-p))
                                                             (when write-p
                                                               (if value-changed-p
                                                                   new-value
                                                                   value)))
                                                         (funcall %replacer nil value))
                                                       value))
                            (case context
                              (:array      (setf (car %stack) :array-value))
                              (:object-key (setf (car %stack) :object-value))
                              ((nil)       (push :complete %stack))))
            (setf %ref-stack prev-stack)))))
    writer)
  (:method ((writer json-writer) value)
    (let ((coerce-key (slot-value writer '%coerce-key))
          (fields (coerced-fields value)))
      (with-object writer
        (loop :for (name value . type-cell) :in fields
              :for type := (if type-cell (car type-cell) t)
              :for key := (funcall coerce-key name)
              :when key ; TODO - Should we error instead of omitting the key?
                :do
                   (let ((coerced-value (or value (cond
                                                    ((and (subtypep 'boolean type) (subtypep type 'boolean))
                                                     nil)
                                                    ((and (subtypep 'list type) (subtypep type 'list))
                                                     #())
                                                    (t 'null)))))
                     (write-key writer key)
                     (write-value writer coerced-value))))))
  (:method ((writer json-writer) (value (eql 't)))
    (%write-atom-value writer value))
  (:method ((writer json-writer) (value (eql 'nil)))
    (%write-atom-value writer value))
  (:method ((writer json-writer) (value (eql 'null)))
    (%write-atom-value writer value))
  (:method ((writer json-writer) (value number))
    (%write-atom-value writer value))
  ;; TODO - Double-check any edge-cases with ~F and if ~E might be more appropriate
  (:method ((writer json-writer) (value real))
    (%write-atom-value writer value))
  (:method ((writer json-writer) (value string))
    (%write-atom-value writer value))
  (:method ((writer json-writer) (value symbol))
    (%write-atom-value writer (string value)))
  (:method  ((writer json-writer) (value pathname))
    (%write-atom-value writer (uiop:native-namestring value)))
  (:method ((writer json-writer) (value cons))
    ;; Try and guess alist/plist
    ;; TODO - this might be too hacky/brittle to have on by default
    (let* ((coerce-key (slot-value writer '%coerce-key))
           (replacer (slot-value writer '%replacer))
           (alist-keys (and (every #'consp value)
                            (loop :for (k . v) :in value
                                  :for key := (and (or (characterp k) (symbolp k) (stringp k))
                                                   (funcall coerce-key k))
                                  :unless key
                                    :return nil
                                  :collect key)))
           (plist-keys (and (null alist-keys)
                            (loop :for cell :on value :by #'cddr
                                  :for (k . rest) := cell
                                  :for key := (and (or (characterp k) (stringp k) (symbolp k))
                                                   (funcall coerce-key k))
                                  :unless (and (consp rest) key)
                                    :return nil
                                  :collect key))))
      (cond
        (alist-keys
         (with-object writer
           (loop :for (k . v) :in value
                 :for key :in alist-keys
                 :do
                    (if replacer
                      (multiple-value-call
                          (lambda (write-p &optional (new-value nil value-changed-p))
                            (when write-p
                              (write-key writer key)
                              (if value-changed-p
                                  (write-value writer new-value)
                                  (write-value writer v))))
                        (funcall replacer key v))
                        (progn
                          (write-key writer key)
                          (write-value writer v))))))
        (plist-keys
         (with-object writer
           (loop :for (k v . rest) :on value :by #'cddr
                 :for key :in plist-keys
                 :do
                    (if replacer
                          (multiple-value-call
                              (lambda (write-p &optional (new-value nil value-changed-p))
                                (when write-p
                                  (write-key writer key)
                                  (if value-changed-p
                                      (write-value writer new-value)
                                      (write-value writer v))))
                            (funcall replacer key v))
                        (progn
                          (write-key writer key)
                          (write-value writer v))))))
        ((listp (cdr value))
         ;; If it looks like a proper list, then consider it a list
         (with-array writer
           (loop :for x :in value
                 :do (write-value writer x))))
        (t
         ;; Otherwise consider it a 2-element tuple
         (with-array writer
           (write-value writer (car value))
           (write-value writer (cdr value)))))))
  (:method ((writer json-writer) (value sequence))
    (with-array writer
      (let ((replacer (slot-value writer '%replacer)))
        (if replacer
            ;; Apply the replacer to each value in the array, with the index as its key
            (map nil
                 (let ((i 0))
                   (lambda (x)
                     (multiple-value-call (lambda (write-p &optional (new-value nil value-changed-p))
                                            (when write-p
                                              (if value-changed-p
                                                  (write-value writer new-value)
                                                  (write-value writer x))))
                       (funcall replacer i x))
                     (incf i)))
                 value)
            (map nil
                 (lambda (x)
                   (write-value writer x))
                 value)))))
  (:method ((writer json-writer) (value hash-table))
    (with-object writer
      (maphash (lambda (key value)
                 (let ((replacer (slot-value writer '%replacer)))
                   (if replacer
                       (multiple-value-call
                           (lambda (write-pair &optional (new-value nil value-changed-p))
                             (when write-pair
                               (write-key writer key)
                               (if value-changed-p
                                   (write-value writer new-value)
                                   (write-value writer value))))
                         (funcall replacer key value))
                       (progn
                         (write-key writer key)
                         (write-value writer value)))))
             value))))

;;; Additional convenience functions/macros
(defun write-values (writer &rest values)
  "Convenience function to write multiple `values' to `writer'. Must be writing an array."
  (declare (dynamic-extent values))
  (check-type writer json-writer)
  (unless (or values (member (slot-value writer '%stack) '(:array :array-value)))
    (error "Attempting to write multiple values outside of an array."))
  (map nil (lambda (x) (write-value writer x)) values)
  writer)

(defun write-array (writer &rest values)
  "Write an array from a series of `values.'"
  (with-array writer
    (apply #'write-values writer values)))

(defun write-property (writer key value)
  "Write an object property/key value pair."
  (check-type writer json-writer)
  (write-key writer key)
  (write-value writer value))

(defun write-properties (writer &rest kvp)
  "Write an set of object properties to `writer'. Must currently be writing an object.
 Ex.
   (with-object writer
     (write-properties writer
                       :speed 10
                       :colour :red)
     (write-key writer :an-array)
     (with-array writer
       (write-value 42))

     (write-properties writer :more 0 :after 42))

see `write-property'
see `write-object'"
  (declare (dynamic-extent kvp))
  (check-type writer json-writer)
  (loop
    :for cell :on kvp :by #'cddr
    :for (k . rest) := cell
    :unless (consp rest)
      :do (error "Malformed property list ~A" (copy-list kvp))
    :do (write-property writer k (car rest)))
  writer)

(defun write-object (writer &rest kvp)
  "Write an object value from a set of key-value pairs.
 Ex.
   (write-object writer
                 :speed 10
                 :colour :red)
"
  (declare (dynamic-extent kvp))
  (check-type writer json-writer)
  (with-object writer
    (apply #'write-properties writer kvp)))

;; dynavar-based write functions
(defvar *writer*)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (documentation '*writer* 'variable) "The active `json-writer' for the various `write-*' functions."))

(defmacro with-writer* ((&rest args &key &allow-other-keys) &body body)
  "Create a new `json-writer' using `args' and bind it to `*writer*'

  `args' are a the same as `make-json-writer'"
  (let ((writer-sym (gensym "WRITER")))
    `(let* ((,writer-sym (make-json-writer ,@args))
            (*writer* ,writer-sym))
       ,@body
       ,writer-sym)))

(defun write-value* (value)
  "As `write-value', but using the currently bound `*writer*'."
  (write-value *writer* value))

(defun begin-array* ()
  "As `begin-array', but using the currently bound `*writer*'."
  (begin-array *writer*))

(defun write-values* (&rest values)
  "As `write-values', but using the currently bound `*writer*'."
  (apply #'write-values *writer* values))

(defun end-array* ()
  "As `end-array', but using the currently bound `*writer*'."
  (end-array *writer*))

(defmacro with-array* (&body body)
  "As `with-array', but using the currently bound `*writer*'."
  `(with-array *writer* ,@body))

(defun write-array* (&rest values)
  "As `write-array', but using the currently bound `*writer*'."
  (apply #'write-array *writer* values))

(defun begin-object* ()
  "As `begin-object', but using the currently bound `*writer*'."
  (begin-object *writer*))

(defun write-key* (key)
  "As `write-key', but using the currently bound `*writer*'."
  (write-key *writer* key))

(defun write-property* (key value)
  "As `write-property', but using the currently bound `*writer*'."
  (write-property *writer* key value))

(defun write-properties* (&rest kvp)
  "As `write-properties', but using the currently bound `*writer*'."
  (apply #'write-properties *writer* kvp))

(defun end-object* ()
  "As `end-object', but using the currently bound `*writer*'."
  (end-object *writer*))

(defmacro with-object* (&body body)
  "As `with-object', but using the currently bound `*writer*'."
  `(with-object *writer* ,@body))

(defun write-object* (&rest kvp)
  "As `write-object', but using the currently bound `*writer*'."
  (apply #'write-object *writer* kvp))

(defun stringify (element &key stream replacer pretty (coerce-key #'coerce-key))
  "Serialize `element' into JSON.
 Returns a fresh string if `stream' is nil, nil otherwise.
  `:stream' like the `destination' in `format', or a `pathname'
  `:pretty' if true, pretty-format the output
  `:replacer' a function which takes a key and value as an argument, and returns t or nil, indicating whether the KV pair should be written.
    - Optionally returns a second value, indicating the value to be stringified in place of the given value.
  `:coerce-key' is a function of one argument, and is used to coerce object keys into non-nil string designators

 see `coerce-key'
"
  (check-type replacer (or symbol function))
  (when replacer
    (setf replacer (%ensure-function replacer)))
  (check-type coerce-key (or symbol function))
  (let ((coerce-key (%ensure-function coerce-key)))
    (flet ((stringify-to (stream)
             (let ((writer (make-json-writer :stream stream
                                             :coerce-key coerce-key
                                             :pretty (and pretty t)
                                             :replacer replacer)))
               (write-value writer element))))
      (cond
        ((null stream)
         (with-output-to-string (stream)
           (stringify-to stream)))
        ((stringp stream)
         (unless (array-has-fill-pointer-p stream)
           (error 'type-error :datum stream :expected-type '(and string (satisfies array-has-fill-pointer-p))))
         (with-output-to-string (stream stream)
           (stringify-to stream))
         nil)
        ((pathnamep stream)
         (with-open-file (stream stream :direction :output
                                        :if-does-not-exist :create
                                        :if-exists :overwrite
                                        :external-format :utf-8)
           (stringify-to stream))
         nil)
        (t
         (let* ((stream (etypecase stream
                          ((eql t) *standard-output*)
                          (stream stream)))
                (stream (cond
                          ((subtypep (stream-element-type stream) 'character) stream)
                          (t (flexi-streams:make-flexi-stream stream :external-format :utf-8)))))
           (stringify-to stream))
         nil)))))
