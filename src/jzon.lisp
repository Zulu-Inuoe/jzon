(defpackage #:com.inuoe.jzon
  (:use #:cl)
  (:export
   #:parse
   #:stringify

   #:json-error
   #:json-parse-error
   #:json-eof-error))

(in-package #:com.inuoe.jzon)

(define-condition json-error (simple-error) ())
(define-condition json-parse-error (json-error) ())
(define-condition json-eof-error (json-parse-error) ())

(declaim (inline %raise))
(defun %raise (type format &rest args)
  (error type :format-control format :format-arguments args))

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

(defun %ends-atom-p (char)
  (or (%whitespace-p char)
      (and (member char '(#\) #\] #\} #\, #\:))
           t)))

(defun %skip-whitespace (peek step)
  "Skip whitespace, and optionally comments, depending on `%*allow-comments*'."
  (flet ((skip-cpp-comment ()
           ;; Skip the first slash.
           (%step step)
           ;; Skip the second slash.
           (let ((c (%step step)))
             (case c
               ((nil)(%raise 'json-eof-error "End of input reading comment exepecting second slash."))
               (#\/ nil)
               (t (%raise 'json-parse-error "Unexpected input '/~A'." c))))

           ;; Skip rest of line or until EOF
           (loop :until (member (%step step) '(nil #\Linefeed #\Return)))))
    (loop :for char := (%peek peek)
          :while (cond
                   ((null char)
                    nil)
                   ((%whitespace-p char)
                    (%step step)
                    t)
                   ((and (char= #\/ char) *%allow-comments*)
                    (skip-cpp-comment)
                    t)))))

(defun %control-char-p (c)
  "Returns true if `c' is a control character per RFC 8259."
  (declare (type character c))
  (<= #x00 (char-code c) #x1F))

(declaim (type (and string (not simple-string)) *%string-accum*))
(defvar *%string-accum*)

(defun %read-json-string (step)
  "Reads a JSON string step-wise using `step' until an unescaped double-quote.
 Returns a `simple-string' representing the string."
  (labels ((interpret (char)
             (cond
               ((char= #\\ char)
                (let ((escaped (%step step)))
                  (case escaped
                    ((nil) (%raise 'json-eof-error "Unexpected end of input after '\\' in string."))
                    ((#\" #\\ #\/) escaped)
                    (#\b #\Backspace)
                    (#\f #\Formfeed)
                    (#\n #\Linefeed)
                    (#\r #\Return)
                    (#\t #\Tab)
                    (#\u (read-unicode))
                    (t (%raise 'json-parse-error "Invalid escape sequence in string '\\~A'." escaped)))))
               ((%control-char-p char)
                (%raise 'json-parse-error "Unexpected control character in string '~A' (~A)." char (char-name char)))
               (t char)))
           (read-unicode ()
             ;; refer to ECMA-404, strings.
             (flet ((read-code-point ()
                      (the (unsigned-byte 32)
                           (loop :for pos :of-type (integer 0 4) :from 0 :below 4
                                 :for weight :of-type (integer 1 4096) := #.(expt 16 3) :then (ash weight -4)
                                 :for digit := (digit-char-p (%step step) 16)
                                 :do (unless digit (%raise 'json-parse-error "Invalid unicode constant in string."))
                                 :sum (the (unsigned-byte 32) (* digit weight)) :of-type (unsigned-byte 32))))
                    (expect-char (char)
                      (let ((c (%step step)))
                        (unless (char= char c)
                          (%raise 'json-parse-error "Expecting ~C, found ~C instead." char c)))))
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
                            (%raise 'json-parse-error "Unexpected UTF-16 surrogate pair: ~A and ~A."
                                    utf-16-high-surrogate-pair
                                    utf-16-low-surrogate-pair))
                          (+ #x010000
                             (ash (- utf-16-high-surrogate-pair #xD800) 10)
                             (- utf-16-low-surrogate-pair #xDC00))))
                      code-point))))))
    (let ((accum *%string-accum*))
      (setf (fill-pointer accum) 0)
      (loop :for next character := (or (%step step) (%raise 'json-eof-error "Encountered end of input inside string constant."))
            :until (char= #\" next)
            :do (vector-push-extend (interpret next) accum))
      (subseq accum 0))))

(defun %read-json-array (peek step read-string)
  "Reads a JSON array of elements until finding a closing brace.
 Returns a `simple-vector' of JSON values."
  (%skip-whitespace peek step)
  (case (%peek peek)
    ((nil) (%raise 'json-eof-error "End of input reading first array element."))
    (#\]
     (%step step)
     (vector))
    (t
     (coerce
      (the list
           (loop
             :collect (%read-json-element peek step read-string)
             :do
                (%skip-whitespace peek step)
                (case (%step step)
                  ((nil) (%raise 'json-eof-error "End of input inside list expecting comma ','."))
                  (#\, nil)
                  (#\] (loop-finish))
                  (t (%raise 'json-parse-error "Expected comma in array.")))))
      'simple-vector))))

(declaim (type hash-table *%key-pool*))
(defvar *%key-pool*)

(defun %read-json-object (peek step read-string)
  (declare (type function read-string))
  (let ((accum (make-hash-table :test 'equal))
        (pool *%key-pool*))
    (flet ((read-key-value ()
             (%skip-whitespace peek step)
             ;; Read quote
             (case (%step step)
               ((nil) (%raise 'json-eof-error "End of input in object. Expected key."))
               (#\" nil)
               (#\}
                (return-from read-key-value nil))
               (t (%raise 'json-parse-error "Expected key in object.")))

             (let* ((key (funcall read-string))
                    (key (or (gethash key pool)
                             (setf (gethash key pool) key))))
               (%skip-whitespace peek step)
               (case (%step step)
                 ((nil) (%raise 'json-eof-error "End of input in object. Expected colon after key '~A'." key))
                 (#\: nil)
                 (t (%raise 'json-parse-error "Expected colon after object key '~A'." key)))

               (setf (gethash key accum) (%read-json-element peek step read-string)))
             t))
      (when (read-key-value)
        (loop
          (%skip-whitespace peek step)
          (case (%step step)
            ((nil) (%raise 'json-eof-error "End of input in object. Expecting comma."))
            (#\, nil)
            (#\} (return))
            (t (%raise 'json-parse-error "Expected comma in object.")))
          (unless (read-key-value)
            (%raise 'json-parse-error "Expected \"key\": value after comma.")))
        accum))))

(defun %number-value (string)
  "Interprets the value of `string' as an RFC 8259 number:
  [ minus ] int [ frac ] [ exp ]
 Returns an `integer', except when frac or exp are present, in which case a `double-float' is returned"
  (declare (type simple-string string))
  (flet ((digit09-p (c)
           (let ((val (- (char-code c) (char-code #\0))))
             (and (<= 0 val 9) val)))
         (digit19-p (c)
           (let ((val (- (char-code c) (char-code #\0))))
             (and (<= 1 val 9) val))))
    (macrolet ((takec (on-eof)
                 "Take the next character, `go'ing to `label' on EOF"
                 `(if (< i len)
                      (prog1 (char string i)
                        (incf i))
                      (go ,on-eof))))
      (prog ((i 0)
             (len (length string))
             (int-sign 1)
             (int-val 0)
             (frac-val 0)
             (frac-len 0)
             (exp-p nil)
             (exp-sign 1)
             (exp-val 0))
         (declare (type (integer 0) int-val frac-val exp-val frac-len)
                  (type (member -1 1) int-sign exp-sign))
         (let ((c (takec :fail)))
           (when (char= c #\-)
             (setf int-sign -1)
             (setf c (takec :fail)))

           (when (char= c #\0)
             (case (takec :done)
               (#\.       (go :parse-frac))
               ((#\e #\E) (go :parse-exp))
               (t         (go :fail))))

           (let ((digit (digit19-p c)))
             (unless digit (go :fail))
             (setf int-val digit)
             (go :parse-int)))

       :parse-int
         (let ((c (takec :done)))
           (when (char= c #\.)
             (go :parse-frac))
           (when (or (char= c #\e)
                     (char= c #\E))
             (go :parse-exp))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf int-val (+ (* int-val 10) digit))))
         (go :parse-int)

       :parse-frac
         (let ((c (takec :fail)))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf frac-val digit)
             (setf frac-len 1)))

       :parse-frac-loop
         (let ((c (takec :done)))
           (when (or (char= c #\e)
                     (char= c #\E))
             (go :parse-exp))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf frac-val (+ (* frac-val 10) digit))
             (incf frac-len)))
         (go :parse-frac-loop)

       :parse-exp
         (let ((c (takec :fail)))
           (setf exp-p t)
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
         (let ((c (takec :done)))
           (let ((digit (digit09-p c)))
             (unless digit (go :fail))
             (setf exp-val (+ (* exp-val 10) digit))))
         (go :parse-exp-loop)

       :done
         (return
           (if (and (not exp-p) (zerop frac-len))
               (* int-sign int-val)
               (let ((exp-mult (expt 10d0 (* exp-sign exp-val))))
                 (+ (* int-sign int-val exp-mult)
                    (/ (* exp-mult frac-val) (expt 10.d0 frac-len))))))
       :fail
         (return nil)))))

(defun %read-json-atom (peek step c)
  "Parse a non-string JSON atom and return its value.
 `c' is the lookahead character already read."
  (let ((accum *%string-accum*))
    (setf (fill-pointer accum) 1)
    (setf (aref accum 0) c)
    (loop
      :for next := (%peek peek)
      :until (or (null next) (%ends-atom-p next))
      :do (vector-push-extend next accum)
          (%step step))
    (cond
      ((string= accum "false") nil)
      ((string= accum "true")  t)
      ((string= accum "null")  'null)
      ((%number-value (subseq accum 0)))
      (t (%raise 'json-parse-error "Unrecognized value in JSON data: '~A'" accum)))))

(declaim (type (integer 0) *%current-depth*))
(defvar *%current-depth*)

(declaim (type (or null (integer 1)) *%maximum-depth*))
(defvar *%maximum-depth*)

(defun %read-json-element (peek step read-string)
  (declare (type function peek step read-string))
  (let ((*%current-depth* (1+ *%current-depth*)))
    (when (and *%maximum-depth* (> *%current-depth* *%maximum-depth*))
      (%raise 'json-parse-error "Maximum depth exceeded."))
    (%skip-whitespace peek step)
    (let ((c (%step step)))
      (case c
        ((nil) (%raise 'json-eof-error "Unexpected end of input."))
        (#\"   (funcall read-string))
        (#\[   (%read-json-array peek step read-string))
        (#\{   (%read-json-object peek step read-string))
        (t     (%read-json-atom peek step c))))))

(defun %read-top-json-element (*%maximum-depth* *%allow-comments* peek step read-string)
  (let ((*%string-accum* (make-array 32 :element-type 'character :adjustable t :fill-pointer 0))
        (*%key-pool* (make-hash-table :test 'equal))
        (*%current-depth* 0))
    (declare (dynamic-extent *%string-accum* *%key-pool* *%current-depth*))
    (prog1 (%read-json-element peek step read-string)
      (%skip-whitespace peek step)
      (or (null (%peek peek))
          (%raise 'json-parse-error "Content after reading object.")))))

(macrolet ((def-make-string-fns (name type)
             `(defun ,name (in)
                "Create peek, step, and read-string functions for the string `in'."
                (declare (type ,type in))
                (let ((i 0)
                      (len (length in)))
                  (declare (type (integer 0 #.array-dimension-limit) i len))
                  (let* ((peek (lambda () (when (< i len) (aref in i))))
                         (step (lambda () (when (< i len) (prog1 (aref in i) (incf i)))))
                         (read-string (lambda ()
                                        (let ((q-pos (position #\" in :start i :end len)))
                                          (unless q-pos (%raise 'json-eof-error "Unexpected end of input reading string."))
                                          (cond
                                            ((null (position #\\ in :start i :end q-pos))
                                             ;; Fast path, just need to check for control chars
                                             (let ((control-char (find-if #'%control-char-p in :start i :end q-pos)))
                                               (when control-char
                                                 (%raise 'json-parse-error "Unexpected control character in string '~A' (~A)." control-char (char-name control-char))))
                                             (prog1 (subseq in i q-pos)
                                               (setf i (1+ q-pos))))
                                            (t
                                             ;; Otherwise we need to worry about escape sequences, unicode, etc.
                                             (%read-json-string step)))))))
                    (values peek
                            step
                            read-string))))))
  (def-make-string-fns %make-fns-simple-string simple-string)
  (def-make-string-fns %make-fns-string (and string (not simple-string))))

(defun %make-fns-stream (in)
  "Create peek, step, and read-string functions for the stream `in'."
  (declare (type stream in))
  (let* ((peek (lambda () (peek-char nil in nil)))
         (step (lambda () (read-char in nil)))
         (read-string (lambda () (%read-json-string step))))
    (values peek
            step
            read-string)))

(defun parse (in &key
                   (maximum-depth 128)
                   (allow-comments nil))
  "Read a JSON value from `in', which may be a string, or a stream.
 `:maximum-depth' controls the maximum depth of the object/array nesting
 `:allow-comments' controls whether or not single-line // comments are allowed."
  (check-type maximum-depth (or (integer 1) null))
  (multiple-value-bind (peek step read-string)
      (etypecase in
        (simple-string (%make-fns-simple-string in))
        (string (%make-fns-string in))
        (stream (%make-fns-stream in)))
    (declare (dynamic-extent peek step read-string))
    (%read-top-json-element maximum-depth (and allow-comments t) peek step read-string)))

(defun %stringify (obj stream)
  "Stringify non-pretty."
  (etypecase obj
    ((eql t)      (write-string "true" stream))
    (null         (write-string "false" stream))
    ((eql null)   (write-string "null" stream))
    (integer      (format stream "~D" obj))
    ;; TODO - Double-check any edge-cases with ~F and if ~E might be more appropriate
    (real         (format stream "~F" obj))
    (string
     (write-char #\" stream)
     (loop :for c :across obj
           :do
              (case c
                ((#\" #\\)
                 (write-char #\\ stream)
                 (write-char c stream))
                (#\Backspace
                 (write-char #\\ stream)
                 (write-char #\b stream))
                (#\Formfeed
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
     (write-char #\" stream))
    (vector
     (write-char #\[ stream)
     (unless (zerop (length obj))
       (%stringify (aref obj 0) stream)
       (loop :for i :from 1 :below (length obj)
             :do (write-char #\, stream)
                 (%stringify (aref obj i) stream)))
     (write-char #\] stream))
    (hash-table
     (write-char #\{ stream)
     (with-hash-table-iterator (iter obj)
       (multiple-value-bind (more? key val) (iter)
         (when more?
           (%stringify (string key) stream)
           (write-char #\: stream)
           (%stringify val stream)
           (loop
             (multiple-value-bind (more? key val) (iter)
               (unless more? (return))
               (write-char #\, stream)
               (%stringify (string key) stream)
               (write-char #\: stream)
               (%stringify val stream))))))
     (write-char #\} stream))))

(defun %needs-lf-separator (element)
  "For pretty-prenting. Returns true if `element' should have a lf separator."
  (typecase element
    ((and vector (not string))
     (some #'%needs-lf-separator element))
    (hash-table
     (or (> (hash-table-count element) 1)
         (block nil
           (maphash (lambda (k v)
                      (declare (ignore k))
                      (when (%needs-lf-separator v)
                        (return t)))
                    element))))))

(defun %indent (stream separator depth)
  "Indent `stream' by outputting `separator' and then spaces for `depth'."
  (write-char separator stream)
  (dotimes (_ (* depth 2))
    (write-char #\Space stream)))

(defun %stringifyp (element stream depth)
  "Stringify Pretty."
  (typecase element
    ((and vector (not string))
     (write-char #\[ stream)
     (unless (zerop (length element))
       (let* ((needs-lf  (%needs-lf-separator element))
              (separator (if needs-lf #\Linefeed #\Space))
              (depth     (if needs-lf (1+ depth) 0)))
         (%indent stream separator depth)
         (%stringifyp (aref element 0) stream depth)
         (loop :for i :from 1 :below (length element)
               :do (write-char #\, stream)
                   (%indent stream separator depth)
                   (%stringifyp (aref element i) stream depth))
         (%indent stream separator (1- depth))))
     (write-char #\] stream))
    (hash-table
     (write-char #\{ stream)
     (with-hash-table-iterator (iter element)
       (multiple-value-bind (more? key val) (iter)
         (when more?
           (let* ((needs-lf  (%needs-lf-separator element))
                  (separator (if needs-lf #\Linefeed #\Space))
                  (depth     (if needs-lf (1+ depth) 0)))
             (%indent stream separator depth)
             (%stringify (string key) stream)
             (write-char #\: stream)
             (write-char #\Space stream)
             (%stringifyp val stream depth)
             (loop
               (multiple-value-bind (more? key val) (iter)
                 (unless more? (return))
                 (write-char #\, stream)
                 (%indent stream separator depth)
                 (%stringify (string key) stream)
                 (write-char #\: stream)
                 (write-char #\Space stream)
                 (%stringifyp val stream depth)))
             (%indent stream separator (1- depth))))))
     (write-char #\} stream))
    (t
     (%stringify element stream))))

(defun stringify (element &key stream pretty)
  "Serialize `element' into JSON. If `stream' is provided, the output is written to it and returns `nil'.
 If `stream' is `nil', a string is created and returned.
  `:stream' is a stream designator to write to, or `nil'
  `:pretty' if true, pretty-format the output"
  (cond
    (stream
     (if pretty
         (%stringifyp element stream 0)
         (%stringify element stream))
     nil)
    (t
     (with-output-to-string (stream)
       (if pretty
           (%stringifyp element stream 0)
           (%stringify element stream))))))
