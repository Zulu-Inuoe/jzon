(defpackage #:com.inuoe.jzon
  (:use #:cl)
  (:export
   ;;; Read
   #:parse
   ;;; Write
   #:stringify

   ;;; Types
   #:json-atom
   #:json-element

   ;;; Conditions
   #:json-error
   #:json-parse-error
   #:json-eof-error

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
   #:write-object)
  (:import-from #:closer-mop)
  (:import-from #:flexi-streams)
  (:import-from #:uiop))

(in-package #:com.inuoe.jzon)

(define-condition json-error (simple-error) ())
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
           (format stream ", position unavailable."))))))
(define-condition json-eof-error (json-parse-error) ())

(deftype json-atom ()
  `(or (eql t)
       (eql nil)
       (eql null)
       real
       string))

(deftype json-element ()
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

(declaim (type boolean *%allow-comments*))
(defvar *%allow-comments*)

(declaim (ftype (function (function) (values (or null character) &optional)) %peek %step)
         (inline %peek %step))
(defun %peek (peek)
  (declare (function peek))
  (values (the (or null character) (funcall peek))))

(defun %step (step)
  (declare (function step))
  (values (the (or null character) (funcall step))))

(defun %whitespace-p (char)
  (and (member char '(#\Space #\Linefeed #\Return #\Tab))
       t))

(defun %skip-whitespace (step)
  "Skip whitespace, and optionally comments, depending on `%*allow-comments*'
 Returns the next character."
  (flet ((skip-cpp-comment ()
           ;; Skip the second slash.
           (let ((c (%step step)))
             (case c
               ((nil)(%raise 'json-eof-error "End of input reading comment exepecting second slash"))
               (#\/ nil)
               (t (%raise 'json-parse-error "Unexpected input '/~A'" c))))

           ;; Skip rest of line or until EOF
           (loop :until (member (%step step) '(nil #\Linefeed #\Return)))))
    (loop :for char := (%step step)
          :do (cond
                ((null char)
                 (return nil))
                ((and (char= #\/ char) *%allow-comments*)
                 (skip-cpp-comment))
                ((not (%whitespace-p char))
                 (return char))))))

(defun %control-char-p (c)
  "Returns true if `c' is a control character per RFC 8259."
  (declare (type character c))
  (<= #x00 (char-code c) #x1F))

(declaim (type (and string (not simple-string)) *%string-accum*))
(defvar *%string-accum*)

(declaim (type (integer 1 (#.array-dimension-limit)) *%max-string-length*))
(defvar *%max-string-length*)

(defun %read-json-string (step)
  "Reads a JSON string step-wise using `step' until an unescaped double-quote.
 Returns a `simple-string' representing the string."
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
    (let ((accum *%string-accum*)
          (max-string-length *%max-string-length*))
      (setf (fill-pointer accum) 0)
      (loop :for next :of-type character := (or (%step step) (%raise 'json-eof-error "Encountered end of input inside string constant"))
            :for i :from 0
            :until (char= #.(char "\"" 0) next)
            :do
               (when (= i max-string-length)
                 (%raise 'json-parse-error "Maximum string length exceeded"))
               (vector-push-extend (interpret next) accum))
      (subseq accum 0))))

(defun %read-json-array (peek step read-string)
  "Reads a JSON array of elements until finding a closing brace.
 Returns a `simple-vector' of JSON values."
  (let ((c (%skip-whitespace step)))
    (case c
      ((nil) (%raise 'json-eof-error "End of input reading first array element"))
      (#\] (vector))
      (t
       (coerce
        (cons (%read-json-element peek step read-string c)
              (loop
                :do (let ((c (%skip-whitespace step)))
                      (case c
                        (#\, nil)
                        (#\] (loop-finish))
                        ((nil) (%raise 'json-parse-error "End of input in array"))
                        (t (%raise 'json-parse-error "Expected comma or ] in array, got '~A'" c))))
                :collect (%read-json-element peek step read-string (%skip-whitespace step))))
        'simple-vector)))))

(declaim (type function *%key-fn*))
(defvar *%key-fn*)

(defun %read-json-object (peek step read-string)
  (declare (type function read-string))
  (let ((accum (make-hash-table :test 'equal))
        (key-fn *%key-fn*))
    (flet ((read-key-value ()
             ;; Read quote
             (case (%skip-whitespace step)
               ((nil) (%raise 'json-eof-error "End of input in object. Expected key"))
               (#.(char "\"" 0) nil)
               (#\}
                (return-from read-key-value nil))
               (t (%raise 'json-parse-error "Expected key in object")))

             (let ((key (funcall key-fn (funcall read-string))))
               (case (%skip-whitespace step)
                 ((nil) (%raise 'json-eof-error "End of input in object. Expected colon after key '~A'" key))
                 (#\: nil)
                 (t (%raise 'json-parse-error "Expected colon after object key '~A'" key)))

               (setf (gethash key accum) (%read-json-element peek step read-string (%skip-whitespace step))))
             t))
      (when (read-key-value)
        (loop
          (case (%skip-whitespace step)
            ((nil) (%raise 'json-eof-error "End of input in object. Expecting comma"))
            (#\, nil)
            (#\} (return))
            (t (%raise 'json-parse-error "Expected comma in object")))
          (unless (read-key-value)
            (%raise 'json-parse-error "Expected \"key\": value after comma"))))
      accum)))

(defun %read-json-number (peek step c)
  "Reads an RFC 8259 number, starting with `c'."
  (flet ((digit09-p (c &aux (val (- (char-code c) (char-code #\0))))
           (and (<= 0 val 9) val))
         (digit19-p (c &aux (val (- (char-code c) (char-code #\0))))
           (and (<= 1 val 9) val))
         (ends-number-p (c)
           (or (%whitespace-p c) (find c "]},"))))
    (macrolet ((takec (on-eof)
                 "Take the next character, `go'ing to `label' on EOF or end of token"
                 `(let ((c (%peek peek)))
                    (when (or (null c) (ends-number-p c))
                      (go ,on-eof))
                    (%step step)
                    c)))
      (prog ((int-sign 1)
             (int-val 0)
             (frac-val 0)
             (frac-len 0)
             (exp-sign 1)
             (exp-val 0))
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
         (return (if (plusp int-sign) 0 -0.0d0))

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
         (return (* int-sign int-val))

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
         (return (* int-sign (+ int-val (/ frac-val (expt 10.d0 frac-len)))))

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
           (let ((exp-mult (expt 10d0 (* exp-sign exp-val))))
             (* int-sign
                (+ (* int-val exp-mult)
                   (/ (* frac-val exp-mult) (expt 10.d0 frac-len))))))
       :fail
         (return nil)))))

(defun %read-json-atom (peek step c)
  "Parse a non-string JSON atom and return its value.
 `c' is the lookahead character already read."
  (flet ((read-until-separator ()
           (loop :for c := (%peek peek)
                 :until (or (null c) (%whitespace-p c) (find c "{}[],-"))
                 :collect c
                 :do (%step step))))
    (macrolet ((expect (string value)
                 `(loop :for i :from 1 :below (length ,string)
                        :for expect-c := (aref ,string i)
                        :for c := (or (%step step)
                                      (%raise 'json-eof-error (format nil "End of input reading token '~A'. Expected '~A'" ,string expect-c)))
                        :unless (char= c expect-c)
                          :do (let ((token (concatenate 'string
                                                        (subseq ,string 0 i)
                                                        (cons c (read-until-separator)))))
                                (%raise 'json-parse-error (format nil "Unexpected token '~A'" token)))
                        :finally (return ,value))))
      (case c
        (#\f (expect "false" nil))
        (#\t (expect "true" t))
        (#\n (expect "null" 'null))
        (t
         ;; Try to read a number
         (or (%read-json-number peek step c)
             (%raise 'json-parse-error "Unrecognized character in JSON data")))))))

(declaim (type (integer 0) *%current-depth*))
(defvar *%current-depth*)

(declaim (type (or null (integer 1)) *%max-depth*))
(defvar *%max-depth*)

(defun %read-json-element (peek step read-string c)
  (declare (type function peek step read-string)
           (type (or null character) c))
  (case c
    ((nil) (%raise 'json-eof-error "Unexpected end of input"))
    (#.(char "\"" 0) (funcall read-string))
    (#\[
     (let ((*%current-depth* (1+ *%current-depth*)))
       (when (and *%max-depth* (> *%current-depth* *%max-depth*))
         (%raise 'json-parse-error "Maximum depth exceeded"))
       (%read-json-array peek step read-string)))
    (#\{
     (let ((*%current-depth* (1+ *%current-depth*)))
       (when (and *%max-depth* (> *%current-depth* *%max-depth*))
         (%raise 'json-parse-error "Maximum depth exceeded"))
       (%read-json-object peek step read-string)))
    (t (%read-json-atom peek step c))))

(macrolet ((def-make-string-fns (name type)
             `(defun ,name (in)
                "Create peek, step, and read-string functions for the string `in'."
                (declare (type ,type in))
                (let ((i 0))
                  (declare (type (integer 0 #.array-dimension-limit) i))
                  (let* ((peek (lambda () (when (< i (length in)) (aref in i))))
                         (step (lambda () (when (< i (length in)) (prog1 (aref in i) (incf i)))))
                         (read-string (lambda ()
                                        (let ((q-pos (position #.(char "\"" 0) in :start i)))
                                          (unless q-pos (%raise 'json-eof-error "Unexpected end of input reading string"))
                                          (cond
                                            ((null (find #\\ in :start i :end q-pos))
                                             ;; Fast path, just need to check for control chars
                                             (let ((control-char (find-if #'%control-char-p in :start i :end q-pos)))
                                               (when control-char
                                                 (%raise 'json-parse-error "Unexpected control character in string '~A' (~A)" control-char (char-name control-char))))
                                             (when (< *%max-string-length* (- q-pos i))
                                               (setf i (+ i (1+ *%max-string-length*)))
                                               (%raise 'json-parse-error "Maximum string length exceeded"))
                                             (prog1 (subseq in i q-pos)
                                               (setf i (1+ q-pos))))
                                            (t
                                             ;; Otherwise we need to worry about escape sequences, unicode, etc.
                                             (%read-json-string step))))))
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
                    (values peek
                            step
                            read-string
                            pos))))))
  (def-make-string-fns %make-fns-simple-string simple-string)
  (def-make-string-fns %make-fns-string (and string (not simple-string))))

(defun %make-fns-stream (in)
  "Create peek, step, and read-string functions for the stream `in'."
  (declare (type stream in))
  (unless (subtypep (stream-element-type in) 'character)
    (return-from %make-fns-stream (%make-fns-stream (flexi-streams:make-flexi-stream in :external-format :utf-8))))
  (let* ((peek (lambda () (peek-char nil in nil)))
         (step (lambda () (read-char in nil)))
         (read-string (lambda () (%read-json-string step)))
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
    (values peek
            step
            read-string
            pos)))

(defun %make-string-pool ()
  "Make a function for 'interning' strings in a pool."
  (let ((pool nil))
    (declare (type list pool))
    (lambda (key)
      (declare (type simple-string key))
      (or (find key pool :test (lambda (s1 s2)
                                 (declare (type simple-string s1 s2))
                                 (string= s1 s2)))
          (progn (push key pool)
                 key)))))

(defun parse (in &key
                   (max-depth 128)
                   (allow-comments nil)
                   (max-string-length (min #x100000 array-dimension-limit))
                   key-fn)
  "Read a JSON value from `in', which may be a vector, a stream, or a pathname.
 `:max-depth' controls the maximum depth of the object/array nesting
 `:allow-comments' controls whether or not single-line // comments are allowed.
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
     (multiple-value-bind (peek step read-string pos)
         (etypecase in
           (simple-string (%make-fns-simple-string in))
           (string (%make-fns-string in))
           (stream (%make-fns-stream in)))
       (declare (dynamic-extent peek step read-string pos))
       (let ((*%max-depth* max-depth)
             (*%allow-comments* (and allow-comments t))
             (*%max-string-length* max-string-length)
             (*%string-accum* (make-array (min 1024 array-dimension-limit) :element-type 'character :adjustable t :fill-pointer 0))
             (*%key-fn* (or (and key-fn (%ensure-function key-fn))
                            (%make-string-pool)))
             (*%current-depth* 0)
             (*%pos-fn* pos))
         (declare (dynamic-extent *%key-fn* *%current-depth* *%pos-fn*))
         (prog1 (%read-json-element peek step read-string (%skip-whitespace step))
           (or (null (%skip-whitespace step))
               (%raise 'json-parse-error "Content after reading element"))))))))

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

;;; Stack is
;;; (<state> . <prev-states>)
;;; where <state> is one of
;;;   - :object
;;;   - :object-key
;;;   - :object-value
;;;   - :array
;;;   - :array-value
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

(defclass json-writer ()
  ((%stream :initarg :stream)
   (%coerce-key :initarg :coerce-key)
   (%pretty :initarg :pretty)
   (%stack :initform nil)
   (%ref-stack :initform nil))
  (:documentation "A JSON writer on which to call `write-value', `begin-object', etc.")
  (:default-initargs
   :stream (make-broadcast-stream)
   :coerce-key #'coerce-key
   :pretty nil))

(defun make-json-writer (&key
                           (stream (make-broadcast-stream))
                           (coerce-key #'coerce-key)
                           (pretty nil))
  "Create a writer for subsequent `write-value', `begin-object', et al calls.
  `:stream' must be a character or binary `stream'
  `:coerce-key' is a function of one argument, and is used to coerce object keys into non-nil string designators

 see `coerce-key'"
  (check-type stream stream)
  (let* ((stream (cond
                   ((subtypep (stream-element-type stream) 'character) stream)
                   (t (flexi-streams:make-flexi-stream stream :external-format :utf-8)))))
    (make-instance 'json-writer :stream stream
                                 :coerce-key (%ensure-function coerce-key)
                                 :pretty (and pretty t))))

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
  (with-slots (%stream %stack) writer
    (case (car %stack)
      ((:array)                     (progn (%write-indentation writer)
                                           (setf (car %stack) :array-value)))
      ((:array-value)               (progn (write-char #\, %stream)
                                           (%write-indentation writer)))
      ((:object-key)                (setf (car %stack) :object-value))
      ((:object :object-value)      (error "Expecting object key")))
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
  (with-slots (%stream %stack %pretty) writer
    (let ((context (car %stack)))
      (case context
        ((:object :object-value))
        (:object-key (error "Attempting to close object before writing key value"))
        (t           (error "Attempting to close object while in ~A" context)))
      (pop %stack)
      (when (eq context :object-value)
        (%write-indentation writer)))
    (write-char #\} %stream))
  writer)

(defmacro with-object ((writer) &body body)
  "Wrapper around `begin-object' and `end-object'."
  (let ((writer-sym (gensym "WRITER")))
    `(let ((,writer-sym ,writer))
       (begin-object ,writer-sym)
       (unwind-protect (progn ,@body)
         (end-object ,writer-sym)))))

(defun begin-array (writer)
  "Begin writing an array to `writer'.

see `write-values'
see `end-array'"
  (check-type writer json-writer)
  (with-slots (%stream %stack) writer
    (case (car %stack)
      ((:array-value)          (progn
                                 (write-char #\, %stream)
                                 (%write-indentation writer)))
      ((:object-key)           (setf (car %stack) :object-value))
      ((:object :object-value) (error "Expecting object key")))
    (push :array %stack)
    (write-char #\[ %stream))
  writer)

(defun end-array (writer)
  "Finish writing an array to `writer'. Must match an opening `begin-array'."
  (check-type writer json-writer)
  (with-slots (%stream %stack) writer
    (let ((context (car %stack)))
      (case context
        ((:array :array-value))
        (t (error "Attempting to close array while in ~A" context)))
      (pop %stack)
      (when (eq context :array-value)
        (%write-indentation writer)))
    (write-char #\] %stream))
  writer)

(defmacro with-array ((writer) &body body)
  (let ((writer-sym (gensym "WRITER")))
    `(let ((,writer-sym ,writer))
       (begin-array ,writer-sym)
       (unwind-protect (progn ,@body)
         (end-array ,writer-sym)))))

(defgeneric write-value (writer value)
  (:documentation "Write a JSON value to `writer'. Specialize this function for customized JSON writing.")
  (:method :around ((writer json-writer) value)
    (with-slots (%stack %ref-stack %pretty) writer
      (let ((context (car %stack)))
        (case context
          ((:object :object-value) (error "Expecting object key")))

        (let ((prev-stack %ref-stack))
          (let ((path (member value prev-stack :test #'eq)))
            (when path
              ;; bail with a ref string
              (write-value writer
                           (let ((*print-circle* t))
                             (format nil "@recursive-ref__ROOT~{->~A~}" (reverse path))))
              (return-from write-value writer)))
          (setf %ref-stack (cons value prev-stack))
          (unwind-protect (progn (call-next-method writer value)
                                 (case context
                                   (:array      (setf (car %stack) :array-value))
                                   (:object-key (setf (car %stack) :object-value))))
            (setf %ref-stack prev-stack)))))
    writer)
  (:method ((writer json-writer) value)
    (let ((coerce-key (slot-value writer '%coerce-key))
          (fields (coerced-fields value)))
      (with-object (writer)
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
         (with-object (writer)
           (loop :for (k . v) :in value
                 :for key :in alist-keys
                 :do
                    (write-key writer key)
                    (write-value writer v))))
        (plist-keys
         (with-object (writer)
           (loop :for (k v . rest) :on value :by #'cddr
                 :for key :in plist-keys
                 :do
                    (write-key writer key)
                    (write-value writer v))))
        ((listp (cdr value))
         ;; If it looks like a proper list, then consider it a list
         (with-array (writer)
           (loop :for x :in value
                 :do (write-value writer x))))
        (t
         ;; Otherwise consider it a 2-element tuple
         (with-array (writer)
           (write-value (car value))
           (write-value (cdr value)))))))
  (:method ((writer json-writer) (value sequence))
    (with-array (writer)
      (map nil
           (lambda (x)
             (write-value writer x))
           value)))
  (:method ((writer json-writer) (value hash-table))
    (with-object (writer)
      (maphash (lambda (key value)
                 (write-key writer key)
                 (write-value writer value))
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

(defun write-property (writer key value)
  "Write an object property/key value pair."
  (check-type writer json-writer)
  (write-key writer key)
  (write-value writer value))

(defun write-properties (writer &rest kvp)
  "Write an set of object properties to `writer'. Must currently be writing an object.
 Ex.
   (with-object (writer)
     (write-properties writer
                       :speed 10
                       :colour :red)
     (write-key writer :an-array)
     (with-array (writer)
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
    :do (write-property writer k (car rest))))

(defun write-object (writer &rest kvp)
  "Write an object value from a set of key-value pairs.
 Ex.
   (write-object writer
                 :speed 10
                 :colour :red)
"
  (declare (dynamic-extent kvp))
  (check-type writer json-writer)
  (with-object (writer)
    (apply #'write-properties writer kvp)))

(defun stringify (element &key stream pretty (coerce-key #'coerce-key))
  "Serialize `element' into JSON.
 Returns a fresh string if `stream' is nil, nil otherwise.
  `:stream' like the `destination' in `format', or a `pathname'
  `:pretty' if true, pretty-format the output
  `:coerce-key' is a function of one argument, and is used to coerce object keys into non-nil string designators

 see `coerce-key'
"
  (check-type coerce-key (or symbol function))
  (let ((coerce-key (%ensure-function coerce-key)))
    (flet ((stringify-to (stream)
             (let ((writer (make-json-writer :stream stream :coerce-key coerce-key :pretty (and pretty t))))
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
