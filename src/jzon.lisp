(defpackage #:com.inuoe.jzon
  (:use #:cl)
  (:export
   ;;; Read
   #:parse

   ;;;; Streaming reader
   #:make-parser
   #:parse-next
   #:close-parser
   #:with-parser

   ;; Reading utils
   #:span

   ;;; Write
   #:stringify

   ;;; Types
   #:json-atom
   #:json-element

   ;;; Conditions
   #:json-error
   #:json-limit-error
   #:json-parse-error
   #:json-parse-limit-error
   #:json-eof-error
   #:json-write-error
   #:json-write-limit-error
   #:json-recursive-write-error

   ;;; Simple extensible writing
   #:coerced-fields
   #:coerce-key

   ;;; Streaming Writer
   #:writer
   #:make-writer
   #:close-writer
   #:with-writer

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
   #:write-object*
   
   ;; Conversion functionality
   #:convert)
  (:local-nicknames
    (#:el #:com.inuoe.jzon/eisel-lemire)
    (#:ie #:introspect-environment)
    (#:rtd #:com.inuoe.jzon/ratio-to-double)
    (#:sf #:com.inuoe.jzon/schubfach)
    (#:tgs #:trivial-gray-streams))
  (:import-from #:closer-mop)
  (:import-from #:flexi-streams)
  (:import-from #:float-features)
  (:import-from #:uiop))

(in-package #:com.inuoe.jzon)

(define-condition json-error (simple-error) ()
  (:documentation "Common error condition for all errors relating to reading/writing JSON."))

(define-condition json-limit-error (json-error)
  ((%limit :initarg :limit
           :reader %json-limit-error-limit))
  (:documentation "Error signalled when some limit in the JSON parser/writer has been exceeded."))

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

(define-condition json-parse-limit-error (json-parse-error json-limit-error)
  ()
  (:report
    (lambda (c stream)
      (apply #'format stream (simple-condition-format-control c) (simple-condition-format-arguments c))
      (format stream " Limit: ~A." (%json-limit-error-limit c))
      (let ((line (%json-parse-error-line c))
           (column (%json-parse-error-column c)))
       (if (and line column)
           (format stream " At line ~A, column ~A." (%json-parse-error-line c) (%json-parse-error-column c))
           (format stream " Position unavailable.")))))
  (:documentation "Error signalled when some limit in the JSON parser has been exceeded."))

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

(declaim (inline %ensure-function))
(defun %ensure-function (x)
  (if (functionp x) x (fdefinition x)))

(declaim (inline %raise))
(defun %raise (type pos format &rest args)
  (declare (type symbol type)
           (type function pos)
           (type string format))
  (if (subtypep type 'json-parse-error)
      (multiple-value-bind (line column) (funcall pos)
        (error type :format-control format :format-arguments args :line line :column column))
      (error type :format-control format :format-arguments args)))

(declaim (inline %raise-limit))
(defun %raise-limit (type pos limit format &rest args)
  (declare (type symbol type)
           (type function pos)
           (type string format))
  (if (subtypep type 'json-parse-error)
    (multiple-value-bind (line column) (funcall pos)
      (error type :format-control format :format-arguments args :line line :column column :limit limit))
    (error type :format-control format :format-arguments args :limit limit)))

(declaim (inline %step))
(defun %step (step)
  (funcall (the (function () (values (or null character) &optional)) step)))

(declaim (inline %read-string))
(defun %read-string (read-string)
  (funcall (the (function () (values simple-string &optional)) read-string)))

(declaim (inline %key-fn))
(defun %key-fn (key-fn str)
  (declare (function key-fn))
  (declare (simple-string str))
  (the (values t &optional) (funcall key-fn str)))

(declaim (inline %whitespace-p))
(defun %whitespace-p (char)
  (and (member char '(#\Space #\Linefeed #\Return #\Tab))
       t))

(declaim (inline %ends-token-p))
(defun %ends-token-p (char)
  (or (%whitespace-p char)
      (and (find char "[]{},/\"")
            t)))

(defun %skip-cpp-comment (step pos)
  (declare (type function step pos))
  ;; Skip the second slash, or open a block comment
  ;; NOTE - We intentionally follow C/C++ behaviour when it comes
  ;;        to disallowing nesting of /**/ style comments.
  (let ((c (%step step)))
    (case c
      ((nil) (%raise 'json-eof-error pos "End of input reading comment exepecting second slash or asterisk."))
      ;; Skip until LF or EOF
      (#\/ (loop :until (member (%step step) '(nil #\Linefeed))))
      (#\*
        ;; Skip until */ or error on EOF
        (prog ((c (%step step)))
          expect-*
          (case c
            ((nil) (%raise 'json-eof-error pos "End of input reading block comment. Expecting '*/'."))
            (#\*   (go expect-/))
            (t     (setf c (%step step))
                   (go expect-*)))
          expect-/
          (setf c (%step step))
          (case c
            ((nil) (%raise 'json-eof-error pos "End of input reading block comment. Expecting '/'."))
            (#\/ nil) ; done
            (t   (go expect-*)))))
      (t (%raise 'json-parse-error pos "Unexpected input '/~A'. Expecting // or /* to begin comment." c)))))

(declaim (inline %skip-whitespace))
(defun %skip-whitespace (step pos c allow-comments)
  "Skip whitespace, and optionally comments, depending on `%*allow-comments*'
 Returns the next character."
  (declare (type function step pos)
           (type (or null character) c)
           (type boolean allow-comments))
  (loop :for char := c :then (%step step)
        :do (cond
              ((null char)                            (return nil))
              ((and (char= #\/ char) allow-comments)  (%skip-cpp-comment step pos))
              ((not (%whitespace-p char))             (return char)))))

(declaim (inline %control-char-p))
(defun %control-char-p (c)
  "Returns true if `c' is a control character per RFC 8259."
  (declare (type character c))
  (<= #x00 (char-code c) #x1F))

(defun %read-unicode (step pos)
  (declare (type function step pos))
  ;; refer to ECMA-404, strings.
  (macrolet ((%read-code-point ()
              `(logior (ash (or (digit-char-p (or (%step step) (%raise 'json-eof-error pos "Unexpected end of input reading unicode escape code in string")) 16)
                               (%raise 'json-parse-error pos "Non-digit in unicode escape code in string"))
                           12)
                      (ash (or (digit-char-p (or (%step step) (%raise 'json-eof-error pos "Unexpected end of input reading unicode escape code in string")) 16)
                               (%raise 'json-parse-error pos "Non-digit in unicode escape code in string"))
                           8)
                      (ash (or (digit-char-p (or (%step step) (%raise 'json-eof-error pos "Unexpected end of input reading unicode escape code in string")) 16)
                               (%raise 'json-parse-error pos "Non-digit in unicode escape code in string"))
                           4)
                      (ash (or (digit-char-p (or (%step step) (%raise 'json-eof-error pos "Unexpected end of input reading unicode escape code in string")) 16)
                               (%raise 'json-parse-error pos "Non-digit in unicode escape code in string"))
                           0))))
    (let ((code-point (%read-code-point)))
      (code-char
       (if (<= #xD800 code-point #xDBFF)
         (let ((hi code-point))
           (let ((c (or (%step step) (%raise 'json-eof-error pos "Unexpected end of input reading low surrogate pair in string"))))
              (unless (char= #\\ c) (%raise 'json-parse-error pos "Expecting \\u after high surrogate pair, found '~A'" c)))
           (let ((c (or (%step step) (%raise 'json-eof-error pos "Unexpected end of input reading low surrogate pair in string"))))
              (unless (char= #\u c) (%raise 'json-parse-error pos "Expecting \\u after high surrogate pair, found '\\~A'" c)))
           (let ((lo (%read-code-point)))
             (unless (<= #xDC00 lo #xDFFF)
               (%raise 'json-parse-error pos "Unexpected UTF-16 surrogate pair: ~4,'0X and ~4,'0X" hi lo))
             (logior  #x10000 (ash (- hi #xD800) 10) (- lo #xDC00))))
         code-point)))))

(defun %read-json-string (step pos string-accum max-string-length base-string-p)
  "Reads a JSON string step-wise using `step' until an unescaped double-quote.
 Returns a `simple-string' representing the string."
  (declare (type function step pos))
  (declare (type (and string (not simple-string)) string-accum))
  (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
  (labels ((interpret (char step pos)
             (cond
               ((char= #\\ char)
                (let ((escaped (%step step)))
                  (case escaped
                    ((nil) (%raise 'json-eof-error pos "Unexpected end of input after '\\' in string"))
                    ((#.(char "\"" 0) #\\ #\/) escaped)
                    (#\b  #\Backspace)
                    (#\f  #\Page)
                    (#\n  #\Linefeed)
                    (#\r  #\Return)
                    (#\t  #\Tab)
                    (#\u  (%read-unicode step pos))
                    (t    (%raise 'json-parse-error pos "Invalid escape sequence in string '\\~A'" escaped)))))
               ((%control-char-p char)
                (%raise 'json-parse-error pos "Unexpected control character in string '~A' (~A)" char (char-name char)))
               (t char))))
    (declare (inline interpret))
    (loop :for next :of-type character := (or (%step step) (%raise 'json-eof-error pos "Encountered end of input inside string constant"))
          :until (char= #.(char "\"" 0) next)
          :do
             (when (= (fill-pointer string-accum) max-string-length)
               (%raise-limit 'json-parse-limit-error pos max-string-length "Maximum string length exceeded"))
             (let ((interpreted (interpret next step pos)))
               (vector-push-extend interpreted string-accum)
               (when (and base-string-p (not (typep interpreted 'base-char)))
                 (setf base-string-p nil)))
          :finally (return (if (zerop (fill-pointer string-accum))
                             ""
                             (if base-string-p
                               (make-array (fill-pointer string-accum) :element-type 'base-char :initial-contents string-accum)
                               (make-array (fill-pointer string-accum) :element-type 'character :initial-contents string-accum)))))))

(defun %read-json-number (step c)
  "Reads an RFC 8259 number, starting with `c'."
  (declare (type function step))
  (declare (type character c))
  (macrolet ((takec-or-fail ()
               "Take the next character, `go'ing to `label' on EOF or end of token"
               `(let ((c (%step step)))
                  (if (or (null c) (%ends-token-p c)) (return (values nil c)) c)))
               (digit-or-fail (c)
                 `(let ((val (- (char-code ,c) #.(char-code #\0))))
                   (if (<= 0 val 9) val (return (values nil ,c)))))
               (takec-or-done ()
                 `(let ((c (%step step)))
                   (if (or (null c) (%ends-token-p c))
                     (let ((exp10 (+ exp10 (* exp-sign exp-val))))
                       (return  (values
                                  (or (el:make-double mantissa exp10 (minusp sign))
                                  (rtd:ratio-to-double (* mantissa (expt 10 exp10) sign)))
                                  c)))
                     c))))
    (prog ((sign 1)
           (mantissa 0)
           (exp10 0)
           (exp-sign 1)
           (exp-val 0))
       (declare (type (member -1 1) sign))
       (declare (type (integer 0) mantissa))
       (declare (type integer exp10))
       (declare (type (member -1 1) exp-sign))
       (declare (type (integer 0) exp-val))
       (let ((c c))
         (when (char= c #\-)
           (setf sign -1)
           (setf c (takec-or-fail)))

         (if (char= c #\0)
           (let ((c (%step step)))
             (cond
                ((or (null c)
                     (%ends-token-p c)) (return (values (if (plusp sign) 0 -0.0d0) c)))
                ((char= c #\.)          (go parse-frac))
                ((or (char= c #\e)
                     (char= c #\E))     (go parse-exp))
                (t                      (return (values nil c)))))
           (setf mantissa (digit-or-fail c))))

     parse-int-loop
       (let ((c (%step step)))
         (cond
          ((or (null c)
               (%ends-token-p c)) (return (values (* mantissa sign) c)))
          ((char= c #\.)          (go parse-frac))
          ((or (char= c #\e)
               (char= c #\E))     (go parse-exp))
          (t
            (setf mantissa (+ (* mantissa 10) (digit-or-fail c)))
            (go parse-int-loop))))

     parse-frac
       (let ((c (takec-or-fail)))
         (setf mantissa (+ (* mantissa 10) (digit-or-fail c))))

     parse-frac-loop
       (decf exp10)
       (let ((c (takec-or-done)))
         (when (or (char= c #\e) (char= c #\E))
           (go parse-exp))
         (setf mantissa (+ (* mantissa 10) (digit-or-fail c))))
       (go parse-frac-loop)

     parse-exp
       (let ((c (takec-or-fail)))
         (case c
           (#\- (setf c (takec-or-fail))
                (setf exp-sign -1))
           (#\+ (setf c (takec-or-fail))))
         (setf exp-val (digit-or-fail c)))

     parse-exp-loop
      (let ((c (takec-or-done)))
         (setf exp-val (+ (* exp-val 10) (digit-or-fail c))))
      (go parse-exp-loop))))

(defun %calc-pos (step n)
  "Calculate line, column numbers by `step' ping through the input `n' times."
  (declare (type (integer 1) n))
  (loop :with line :of-type (integer 1)  := 1
        :with col :of-type (integer 1) := 1
        :with cr := nil
        :for p :from 0 :below (1- n)
        :for c := (%step step)
        :do (case c
              (#\Linefeed (incf line) (setf col 1))
              (t (incf col)))
        :finally (return (values line col))))

(macrolet ((def-make-string-fns (name type)
             `(defun ,name (in start end max-string-length)
                "Create step, and read-string functions for the string `in'."
                (declare (type ,type in))
                (declare (type (integer 0 (#.array-dimension-limit)) start end))
                (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
                (let ((i start))
                  (declare (type (integer 0 #.array-dimension-limit) i))
                  (let* ((step (lambda () (when (< i end) (char in (shiftf i (1+ i))))))
                         (pos (lambda () (%calc-pos step (- (shiftf i start) start))))
                         (read-string (let ((string-accum (make-array (min 256 (1- array-dimension-limit)) :element-type 'character :adjustable t :fill-pointer 0)))
                                        (lambda ()
                                          ;; Scan until we hit a closing "
                                          ;; Error on EOF
                                          ;; Error if we encounter a literal control char
                                          ;; Track suitable element-type
                                          (loop
                                            :with base-string-p := t
                                            :for j :of-type (integer 1 (#.array-dimension-limit)) :from i
                                            :for len :of-type (integer 0 (#.array-dimension-limit)) :from 0
                                            :do
                                              (when (<= end j)
                                                (setf i end)
                                                (%raise 'json-eof-error pos "Unexpected end of input when reading string."))
                                              (let ((c (char in j)))
                                                (case c
                                                  (#.(char "\"" 0)
                                                    (return
                                                      (prog1 (cond
                                                               ((zerop len) "")
                                                               (base-string-p
                                                                 (loop :with ret := (make-array len :element-type 'base-char)
                                                                       :for k :from 0 :below len
                                                                       :do (setf (char ret k) (char in (+ i k)))
                                                                       :finally (return ret)))
                                                               (t
                                                                 (loop :with ret := (make-array len :element-type 'character)
                                                                       :for k :from 0 :below len
                                                                       :do (setf (char ret k) (char in (+ i k)))
                                                                       :finally (return ret))))
                                                        (setf i (1+ j)))))
                                                  (#\\ 
                                                      ;; we need to worry about escape sequences, unicode, etc.
                                                      ;; Copy over what we have so far
                                                      (when (< (array-dimension string-accum 0) len)
                                                        (adjust-array string-accum (* len 2)))
                                                      (setf (fill-pointer string-accum) len)
                                                      (replace string-accum in :start2 i :end2 j)
                                                      (setf i j)
                                                      (return (%read-json-string step pos string-accum max-string-length base-string-p)))
                                                  (t
                                                    (when (= max-string-length len)
                                                      (setf i (1+ j))
                                                      (%raise-limit 'json-parse-limit-error pos max-string-length "Maximum string length exceeded."))
                                                    (when (%control-char-p c)
                                                      (setf i (1+ j))
                                                      (%raise 'json-parse-error pos "Unexpected control character in string '~A' (~A)" c (char-name c)))

                                                    (when (and base-string-p (not (typep c 'base-char)))
                                                      (setf base-string-p nil))))))))))
                    (values step
                            read-string
                            pos))))))
  (def-make-string-fns %make-fns-simple-string simple-string)
  (def-make-string-fns %make-fns-string (and string (not simple-string))))

(declaim (inline %utf-8-decode))
(defun %utf-8-decode (u1 consume-octet)
  (declare (type (unsigned-byte 8) u1))
  (macrolet ((consume-octet ()
               `(let ((o (funcall (the (function () (values (unsigned-byte 8) &optional)) consume-octet))))
                 (if (< #x7f o #xc0) o (error "Unexpected value #x~2,'0X in UTF-8 sequence." o)))))
    (cond
      ((< u1 #x80) (code-char u1))
      ((< u1 #xc0) (error "Unexpected value #x~2,'0X at start of UTF-8 sequence." u1))
      (t
        (let ((u2 (consume-octet)))
          (cond
            ((< u1 #xc2) (error "Overlong UTF-8 sequence."))
            ((< u1 #xe0)
              (code-char (logior (ash (logand u1 #x1f) 6)
                                 (ash (logxor u2 #x80) 0))))
            (t
              (let ((u3 (consume-octet)))
                (cond
                  ((and (= u1 #xe0) (< u2 #xa0)) (error "Overlong UTF-8 sequence."))
                  ((< u1 #xf0)
                    (let ((start (logior (ash (logand u1 #x0f) 12)
                                         (ash (logand u2 #x3f) 6))))
                      (if (<= #xd800 start #xdfc0)
                        (error "Character out of range in UTF-8 sequence.")
                        (code-char (logior start (logand u3 #x3f))))))
                  (t ; 4 octets
                    (let ((u4 (consume-octet)))
                      (cond
                        ((and (= u1 #xf0) (< u2 #x90)) (error "Overlong UTF-8 sequence."))
                        ((< u1 #xf8)
                          (if (or (> u1 #xf4) (and (= u1 #xf4) (> u2 #x8f)))
                            (error "Character out of range in UTF-8 sequence.")
                            (code-char (logior (ash (logand u1 #x07) 18)
                                               (ash (logxor u2 #x80) 12)
                                               (ash (logxor u3 #x80) 6)
                                               (ash (logxor u4 #x80) 0)))))
                        ;; from here on we'll be getting either
                        ;; invalid continuation bytes or overlong
                        ;; 5-byte or 6-byte sequences.
                        (t
                          (consume-octet)
                          (cond
                            ((and (= u1 #xf8) (< u2 #x88)) (error "Overlong UTF-8 sequence."))
                            ((< u1 #xfc) (error "Character out of range in UTF-8 sequence."))
                            (t
                              (consume-octet)
                              (cond
                                ((and (= u1 #xfc) (< u2 #x84)) (error "Overlong UTF-8 sequence."))
                                (t (error "Character out of range in UTF-8 sequence."))))))))))))))))))

(macrolet ((def-make-ub8-fns (name type)
             `(defun ,name (in start end max-string-length)
               (declare (type ,type in))
               (declare (type (integer 0 (#.array-dimension-limit)) start end))
               (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
               (let ((i start))
                 (declare (type (integer 0 #.array-dimension-limit) i))
                 (let* ((consume-octet (lambda () (if (< i end) (aref in (shiftf i (1+ i))) (error "End of data while in UTF-8 sequence."))))
                        (step (lambda () (when (< i end) (%utf-8-decode (aref in (shiftf i (1+ i))) consume-octet))))
                        (pos (lambda () (%calc-pos step (- (shiftf i start) start))))
                        (read-string (let ((string-accum (make-array (min 256 (1- array-dimension-limit)) :element-type 'character :adjustable t :fill-pointer 0)))
                                       (lambda ()
                                         (setf (fill-pointer string-accum) 0)
                                         (%read-json-string step pos string-accum max-string-length t)))))
                   (values step read-string pos))))))
  (def-make-ub8-fns %make-fns-simple-array-ub8 (simple-array (unsigned-byte 8) (*)))
  (def-make-ub8-fns %make-fns-array-ub8 (and (array (unsigned-byte 8) (*)) (not (simple-array (unsigned-byte 8) (*))))))

(defun %make-fns-binary-stream (in max-string-length)
  (declare (type stream in))
  (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
  (let* ((consume-octet (lambda () (or (read-byte in nil nil)
                                       (error "End of data while in UTF-8 sequence."))))
         (step (lambda () (let ((u1 (read-byte in nil nil))) (when u1 (%utf-8-decode u1 consume-octet)))))
         (pos (lambda ()
                (let ((n (ignore-errors (file-position in))))
                  (if (and n (ignore-errors (file-position in 0)))
                    (multiple-value-bind (line col) (%calc-pos step n)
                      (file-position in n)
                      (values line col))
                    (values nil nil)))))
         (read-string (let ((string-accum (make-array (min 256 (1- array-dimension-limit)) :element-type 'character :adjustable t :fill-pointer 0)))
                        (lambda ()
                          (setf (fill-pointer string-accum) 0)
                          (%read-json-string step pos string-accum max-string-length t)))))
    (values step read-string pos)))

(defun %make-fns-stream (in max-string-length)
  "Create step, and read-string functions for the stream `in'."
  (declare (type stream in))
  (declare (type (integer 1 (#.array-dimension-limit)) max-string-length))
  (let* ((step (lambda () (read-char in nil)))
         (pos (lambda ()
                (let ((n (ignore-errors (file-position in))))
                  (if (and n (ignore-errors (file-position in 0)))
                    (multiple-value-bind (line col) (%calc-pos step n)
                      (file-position in n)
                      (values line col))
                    (values nil nil)))))
         (read-string (let ((string-accum (make-array (min 256 (1- array-dimension-limit)) :element-type 'character :adjustable t :fill-pointer 0)))
                        (lambda ()
                          (setf (fill-pointer string-accum) 0)
                          (%read-json-string step pos string-accum max-string-length t)))))
    (values step
            read-string
            pos)))

(defstruct (%parser-state
             (:constructor %make-parser-state)
             (:conc-name nil)
             (:copier nil)
             (:predicate nil))
  (%parser-state-state 'toplevel :type symbol)
  (%parser-state-lookahead nil :type (or null character))
  (%parser-state-context nil :type list))

(defclass parser ()
  ((%step
    :type function)
   (%read-string
    :type function)
   (%pos
    :type function)
   (%allow-comments
    :initform nil
    :type boolean)
   (%allow-trailing-comma
    :initform nil
    :type boolean)
   (%allow-multiple-content
    :initform nil
    :type boolean)
   (%key-fn
    :type function)
   (%max-string-length
    :initform (min #x100000 (1- array-dimension-limit))
    :type (integer 1 (#.array-dimension-limit)))
   (%close-action
    :type (or null function))
   (%parser-state
    :initform (%make-parser-state)
    :type %parser-state))
  (:documentation "An incremental JSON parser.

see `make-parser'
see `next'
see `close-parser'"))

(defclass %span ()
  ((%vector :initarg :vector)
   (%start :initarg :start)
   (%end :initarg :end)))

(defclass %string-span (%span) ())
(defclass %octet-vector-span (%span) ())

(declaim (inline %make-fns))
(defun %make-fns (in max-string-length)
  "Create the step, read-string, and pos functions for `in'.

see `%step'
see `%read-string'"
  (labels ((recurse (in start end)
            (etypecase in
              (simple-string  (%make-fns-simple-string in start (or end (length in)) max-string-length))
              (string         (%make-fns-string in start (or end (length in)) max-string-length))
              (stream         (if (subtypep (stream-element-type in) 'character)
                                (%make-fns-stream in max-string-length)
                                (%make-fns-binary-stream in max-string-length)))
              ((simple-array (unsigned-byte 8) (*)) (%make-fns-simple-array-ub8 in start (or end (length in)) max-string-length))
              ((array (unsigned-byte 8) (*))        (%make-fns-array-ub8 in start (or end (length in)) max-string-length))
              (%string-span       (with-slots (%vector %start %end) in
                                    (recurse %vector %start %end)))
              (%octet-vector-span (with-slots (%vector %start %end) in
                                    (recurse %vector %start %end))))))
    (recurse in 0 nil)))

(defun %make-string-pool ()
  "Make a function for 'interning' strings in a pool."
  (let ((pool (list "")))
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

(defun make-parser (in &key
                      (allow-comments nil)
                      (allow-trailing-comma nil)
                      (allow-multiple-content nil)
                      (max-string-length (min #x100000 (1- array-dimension-limit)))
                      (key-fn t))
  "Construct a `parser' Read a JSON value from `in', which may be a vector, a stream, or a pathname.
 `:allow-comments' controls if we allow single-line // comments and /**/ multiline block comments.
 `:allow-trailing-comma' controls if we allow a single comma `,' after all elements of an array or object.
 `:allow-multiple-content' controls if we alow extra content beyond a single toplevel JSON value.
 `:max-string-length' controls the maximum length allowed when reading a string key or value.
 `:key-fn' is a function of one value which 'pools' object keys, or `nil' to disable pooling, and `t' for the default pool.

see `next'
see `close-parser'"
  (check-type max-string-length (or boolean (integer 1 (#.array-dimension-limit))))
  (check-type key-fn (or boolean symbol function))

  (multiple-value-bind (input close-action)
      (typecase in
        (pathname
          (let ((f (open in :direction :input :external-format :utf-8)))
            (values f (lambda () (close f)))))
        (t (values in (lambda ()))))
    (let ((parser (make-instance 'parser))
          (max-string-length (case max-string-length
                               ((nil) (1- array-dimension-limit))
                               ((t)   #x100000)
                               (t     max-string-length))))
      (with-slots (%step %read-string %pos %allow-comments %allow-trailing-comma %allow-multiple-content %max-string-length %key-fn %close-action %parser-state) parser
        (setf %close-action close-action)
        (setf (values %step %read-string %pos) (%make-fns input max-string-length))

        (setf %allow-comments (and allow-comments t))
        (setf %allow-trailing-comma (and allow-trailing-comma t))
        (setf %allow-multiple-content (and allow-multiple-content t))
        (setf %key-fn (etypecase key-fn
                        (null     #'identity)
                        ((eql t)  (%make-string-pool))
                        (function key-fn)
                        (symbol   (let ((sym key-fn))
                                    (lambda (str)
                                      (funcall (symbol-function sym) str)))))))
      parser)))

(defun close-parser (parser)
  "Close the `parser'"
  (check-type parser parser)
  (let ((action (shiftf (slot-value parser '%close-action) nil)))
    (when action
      (funcall (the function action))
      (slot-makunbound parser '%step)
      (slot-makunbound parser '%read-string)
      (slot-makunbound parser '%pos)
      (slot-makunbound parser '%key-fn)))
  parser)

(defmacro with-parser ((var &rest args) &body body)
  "Create a `parser', ensuring `close-parser' is called on it on exit.

see `make-parser'
see `close-parser'"
  (let ((parser-sym (gensym (string '#:parser))))
    `(let ((,parser-sym (make-parser ,@args)))
      (unwind-protect (let ((,var ,parser-sym)) ,@body)
        (close-parser ,parser-sym)))))

(declaim (inline %parse-next))
(defun %parse-next (%parser-state %step %read-string %pos %key-fn %allow-trailing-comma %allow-comments %allow-multiple-content)
  (declare (type %parser-state %parser-state)
           (type function %step %read-string %pos %key-fn)
           (type boolean %allow-trailing-comma)
           (type boolean %allow-comments))
  (labels ((read-element (lc)
            (declare (type character lc))
            (macrolet ((expect (string value)
                         (declare (type simple-string string))
                         `(progn
                            ,@(loop :for i :from 1 :below (length string)
                                    :for expect-c := (char string i)
                                    :collect `(let ((lc (or (%step %step)
                                                           (%raise 'json-parse-error %pos ,(concatenate 'string "Unexpected token '" (subseq string 0 i) "'")))))
                                                (unless (char= lc ,expect-c)
                                                  (%raise 'json-parse-error %pos "Unexpected token '~A'" (concatenate 'string
                                                                                                                 ,(subseq string 0 i)
                                                                                                                 (loop :for c := lc :then (%step %step)
                                                                                                                       :until (or (null c) (%ends-token-p c))
                                                                                                                       :collect c
                                                                                                                       :finally (setf (%parser-state-lookahead %parser-state) c)))))))
                            (let ((lc (%step %step)))
                              (unless (or (null lc) (%ends-token-p lc))
                                (%raise 'json-parse-error %pos "Unexpected token '~A'" (concatenate 'string
                                                                                                    ,string
                                                                                                    (loop :for c := lc :then (%step %step)
                                                                                                          :until (or (null c) (%ends-token-p c))
                                                                                                          :collect c
                                                                                                          :finally (setf (%parser-state-lookahead %parser-state) c)))))
                              (setf (%parser-state-lookahead %parser-state) lc))
                            (setf (%parser-state-state %parser-state) (car (%parser-state-context %parser-state)))
                            (values :value ,value))))
              (case lc
                (#\[              (push-state :begin-array))
                (#\{              (push-state :begin-object))
                (#.(char "\"" 0)  (let ((value (%read-string %read-string)))
                                    (setf (%parser-state-lookahead %parser-state) nil)
                                    (setf (%parser-state-state %parser-state) (car (%parser-state-context %parser-state)))
                                    (values :value value)))
                (#\f              (expect "false" nil))
                (#\t              (expect "true" t))
                (#\n              (expect "null" 'null))
                (t                (multiple-value-bind (number lookahead) (%read-json-number %step lc)
                                    (unless number
                                      (if lookahead
                                        (%raise 'json-parse-error %pos "Unexpected character in JSON data '~C' (~A)" lookahead (char-name lookahead))
                                        (%raise 'json-eof-error %pos "End of input when reading number")))

                                    (setf (%parser-state-lookahead %parser-state) lookahead)
                                    (setf (%parser-state-state %parser-state) (car (%parser-state-context %parser-state)))
                                    (values :value number))))))
            (push-state (kind)
              (setf (%parser-state-lookahead %parser-state) nil)
              (setf (%parser-state-state %parser-state) kind)
              (values kind nil))
            (pop-state (kind)
              (pop (%parser-state-context %parser-state))
              (setf (%parser-state-state %parser-state) (car (%parser-state-context %parser-state)))
              (values kind nil)))
    (declare (dynamic-extent #'read-element #'push-state #'pop-state))
    (ecase (%parser-state-state %parser-state)
      (toplevel
        (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
          (cond
            ((null lc)  (%raise 'json-eof-error %pos "End of input when reading JSON element"))
            (t          (read-element lc)))))
      (:begin-array
        (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
          (case lc
            ((nil)  (%raise 'json-eof-error %pos "End of input when reading array, expecting element or array close"))
            (#\]    (setf (%parser-state-state %parser-state) (car (%parser-state-context %parser-state)))
                    (values :end-array nil))
            (t      (push 'after-read-array-element (%parser-state-context %parser-state))
                    (read-element lc)))))
      (after-read-array-element
        (let ((lc (%skip-whitespace %step %pos (let ((lc (%parser-state-lookahead %parser-state)))
                                                 (if lc
                                                   (progn (setf (%parser-state-lookahead %parser-state) nil)
                                                          lc)
                                                   (%step %step)))
                                    %allow-comments)))
          (case lc
            ((nil)  (%raise 'json-eof-error %pos "End of input when reading array, expecting comma or array close"))
            (#\,    (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
                      (case lc
                        ((nil)  (if %allow-trailing-comma
                                  (%raise 'json-eof-error %pos "End of input when reading array, expecting element or array close")
                                  (%raise 'json-eof-error %pos "End of input when reading array, expecting element")))
                          (#\]  (unless %allow-trailing-comma
                                  (%raise 'json-parse-error %pos "Trailing comma when reading array"))
                                (pop-state :end-array))
                          (t    (read-element lc)))))
            (#\]    (pop-state :end-array))
            (t      (%raise 'json-parse-error %pos "Unexpected character '~A' (~A) when reading array, expecting comma or array close" lc (char-name lc))))))
      (:begin-object
        (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
          (case lc
            ((nil)            (%raise 'json-eof-error %pos "End of input when reading object, expecting key or object close"))
            (#\}              (setf (%parser-state-state %parser-state) (car (%parser-state-context %parser-state)))
                              (values :end-object nil))
            (#.(char "\"" 0)  (setf (%parser-state-state %parser-state) 'after-read-key)
                              (push 'after-read-property (%parser-state-context %parser-state))
                              (values :object-key (%key-fn %key-fn (%read-string %read-string))))
            (t                (%raise 'json-parse-error %pos "Unexpected character '~A' (~A) when reading object, expecting key" lc (char-name lc))))))
      (after-read-key
        (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
          (case lc
            ((nil) (%raise 'json-eof-error %pos "End of input when reading object, expecting colon after object key"))
            (#\:)
            (t     (%raise 'json-parse-error %pos "Unexpected character '~A' (~A) when reading object, expecting colon after object key" lc (char-name lc)))))

        (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
          (case lc
            ((nil)  (%raise 'json-eof-error %pos "End of input when reading object, expecting value after colon"))
            (t      (read-element lc)))))
      (after-read-property
        (let ((lc (%skip-whitespace %step %pos (let ((lc (%parser-state-lookahead %parser-state)))
                                                 (if lc
                                                   (progn (setf (%parser-state-lookahead %parser-state) nil)
                                                          lc)
                                                   (%step %step)))
                                    %allow-comments)))
          (case lc
            ((nil)  (%raise 'json-eof-error %pos "End of input when reading object. Expecting comma or object close"))
            (#\,    (let ((lc (%skip-whitespace %step %pos (%step %step) %allow-comments)))
                      (case lc
                        ((nil)
                          (if %allow-trailing-comma
                            (%raise 'json-eof-error %pos "End of input when reading object. expected key or object close after comma")
                            (%raise 'json-eof-error %pos "End of input when reading object, expected key after comma")))
                        (#\}
                          (unless %allow-trailing-comma
                            (%raise 'json-parse-error %pos "Trailing comma when reading object"))
                          (pop-state :end-object))
                        (#.(char "\"" 0)
                          (setf (%parser-state-state %parser-state) 'after-read-key)
                          (values :object-key (%key-fn %key-fn (%read-string %read-string))))
                        (t
                          (if %allow-trailing-comma
                            (%raise 'json-parse-error %pos "Unexpected character '~A' (~A) when reading object, expecting key or object close after comma" lc (char-name lc))
                            (%raise 'json-parse-error %pos "Unexpected character '~A' (~A) when reading object, expecting key after comma" lc (char-name lc)))))))
            (#\}    (pop-state :end-object))
            (t      (%raise 'json-parse-error %pos "Unexpected character '~A' (~A) when reading object, expecting comma or object close" lc (char-name lc))))))
      ((nil)
        (let ((lc (%skip-whitespace %step %pos (or (%parser-state-lookahead %parser-state) (%step %step)) %allow-comments)))
          (setf (%parser-state-lookahead %parser-state) lc)
          (cond
            ((null lc)                  (values nil nil))
            (%allow-multiple-content    (read-element lc))
            (t                          (%raise 'json-parse-error %pos "Content after reading element"))))))))

(defun parse-next (parser)
  "Read the next token from `parser'.

  Returns 2 values:
    :value, value       ; A `json-atom' <value>
    :begin-array, nil   ; Array open [
    :end-array, nil     ; Array close ]
    :begin-object, nil  ; Object open {
    :object-key, key    ; Object key
    :end-object, nil    ; Object finished }
    nil. nil            ; Parsing complete

see `with-parser'
see `make-parser'
see `close-parser'"
  (check-type parser parser)
  (when (null (slot-value parser '%close-action))
    (error 'json-error :format-control "The parser has been closed."))
  (with-slots (%step %read-string %pos %key-fn %allow-trailing-comma %allow-comments %allow-multiple-content %parser-state) parser
    (%parse-next %parser-state %step %read-string %pos %key-fn %allow-trailing-comma %allow-comments %allow-multiple-content)))

(defun %parse (%step %read-string %pos %key-fn %max-depth %allow-comments %allow-trailing-comma %allow-multiple-content)
  (declare (type function %step %read-string %pos %key-fn))
  (declare (type (integer 1 #xFFFF) %max-depth))
  (declare (type boolean %allow-comments %allow-trailing-comma))
  (let ((%parser-state (%make-parser-state))
        (depth 0)
        top
        stack
        key
        len)
    (declare (dynamic-extent %parser-state stack key))
    (declare (type (integer 0 #xFFFF) depth))
    (declare (type list stack key len))
    (macrolet ((finish-value (value &optional check-lc)
                 `(let ((value ,value))
                    (if (null stack)
                      (if %allow-multiple-content
                        ,(if check-lc
                          `(let ((lc (%parser-state-lookahead %parser-state)))
                            (unless (or (null lc) (%whitespace-p lc))
                              (%raise 'json-parse-error %pos "Non-whitespace character after JSON atom '~A': '~C' (~A)" (stringify value) lc (char-name lc)))
                            (return value))
                          `(return value))
                        (setf top value))
                      (let ((container (car stack)))
                        (if (listp container)
                          (progn (push value (the list (car stack)))
                                 (incf (the (integer 0) (car len))))
                          (setf (gethash (pop key) (the hash-table (car stack))) value))))))
                (inc-depth ()
                  `(progn
                    (when (= depth %max-depth)
                      (%raise-limit 'json-parse-limit-error %pos %max-depth "Maximum depth exceeded."))
                    (incf depth))))
      (loop
        (multiple-value-bind (event value) (%parse-next %parser-state %step %read-string %pos %key-fn %allow-trailing-comma %allow-comments %allow-multiple-content)
          (declare (dynamic-extent event))
          (case event
            ((nil)          (return top))
            (:value         (finish-value value t))
            (:begin-array   (inc-depth)
                            (push (list) stack)
                            (push 0 len))
            (:end-array     (decf depth)
                            (let ((elements (the list (pop stack)))
                                  (length (the (integer 0) (pop len))))
                              (finish-value
                                (if (zerop length)
                                  #()
                                  (loop :with array := (make-array length)
                                        :for i :from (1- length) :downto 0
                                        :for elt :in elements
                                        :do (setf (aref array i) elt)
                                        :finally (return array))))))
            (:begin-object  (inc-depth)
                            (push (make-hash-table :test 'equal) stack))
            (:object-key    (push value key))
            (:end-object    (decf depth)
                            (finish-value (pop stack)))))))))

(defun span (in &key (start 0) end)
  "Define a bounded sequence in `in' for use in `parse' and `make-parser' with a `start' and `end'."
  (check-type start (integer 0 (#.array-dimension-limit)))
  (check-type end (or null (integer 0 (#.array-dimension-limit))))
  (etypecase in
    (string
      (let ((start start)
            (end (or end (length in))))
        (unless (<= 0 start end (length in))
          (error "The bounding indices ~A and ~A are bad for a sequence of length ~A." start end (length in)))
        (make-instance '%string-span :vector in :start start :end end)))
    ((vector (unsigned-byte 8))
      (let ((start start)
            (end (or end (length in))))
        (unless (<= 0 start end (length in))
          (error "The bounding indices ~A and ~A are bad for a sequence of length ~A." start end (length in)))
        (make-instance '%octet-vector-span :vector in :start start :end end)))))

(defun parse (in &key
                   (max-depth 128)
                   (allow-comments nil)
                   (allow-trailing-comma nil)
                   (allow-multiple-content nil)
                   (max-string-length (min #x100000 (1- array-dimension-limit)))
                   (key-fn t))
  "Read a JSON value from `in', which may be a vector, a stream, a pathname, or a span from `span'.
 `:max-depth' controls the maximum depth allowed when nesting arrays or objects.
 `:allow-comments' controls if we allow single-line // comments and /**/ multiline block comments.
 `:allow-trailing-comma' controls if we allow a single comma `,' after all elements of an array or object.
 `:allow-multiple-content' controls if we alow extra content beyond a single toplevel JSON value.
 `:max-string-length' controls the maximum length allowed when reading a string key or value.
 `:key-fn' is a function of one value which 'pools' object keys, or `nil' to disable pooling, and `t' for the default pool."
  (check-type max-depth (or boolean (integer 1 #xFFFF)))
  (check-type key-fn (or boolean symbol function))
  (check-type max-string-length (or boolean (integer 1 (#.array-dimension-limit))))
  (let ((key-fn (etypecase key-fn
                  (null     #'identity)
                  ((eql t)  (%make-string-pool))
                  (function key-fn)
                  (symbol   (fdefinition key-fn))))
        (max-depth (case max-depth
                     ((nil) #xFFFF)
                     ((t)   128)
                     (t     max-depth)))
        (max-string-length (case max-string-length
                             ((t)   #x100000)
                             ((nil) (1- array-dimension-limit))
                             (t     max-string-length))))

    (typecase in
      (pathname
       (with-open-file (in in :direction :input :external-format :utf-8)
         (parse in :max-depth max-depth :allow-comments allow-comments :allow-trailing-comma allow-trailing-comma :allow-multiple-content allow-multiple-content :max-string-length max-string-length :key-fn key-fn)))
      (t
        (multiple-value-bind (%step %read-string %pos) (%make-fns in max-string-length)
          (declare (dynamic-extent %step %read-string %pos))
          (%parse %step %read-string %pos key-fn max-depth (and allow-comments t) (and allow-trailing-comma t) (and allow-multiple-content t)))))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline %type=))
  (defun %type= (a b)
    (and (subtypep a b)
         (subtypep b a))))

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
    (format nil "~D" key))
  (:method ((key float))
    (with-output-to-string (s)
      (macrolet ((#1=#:|| ()
                  `(etypecase key
                    (double-float (sf:write-double key s))
                    (single-float (sf:write-float key s))
                    ,@(unless (%type= 'short-float 'single-float)
                      '((short-float (sf:write-float (coerce key 'single-float) s))))
                    ,@(unless (%type= 'long-float 'double-float)
                      '((long-float (sf:write-double (coerce key 'double-float) s)))))))
        (#1#))))
  (:method ((key ratio))
    (with-output-to-string (s)
      (sf:write-double (rtd:ratio-to-double key) s))))

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

(define-condition json-write-limit-error (json-write-error json-limit-error)
  ()
  (:report
    (lambda (c stream)
      (apply #'format stream (simple-condition-format-control c) (simple-condition-format-arguments c))
      (format stream " Limit: ~A." (%json-limit-error-limit c))))
  (:documentation "Error signalled when a limit on the JSON writer has been exceeded."))

(defclass %string-output-stream (tgs:fundamental-character-output-stream)
  ((%string :initarg :string)))

(defmethod tgs:stream-write-char ((stream %string-output-stream) character)
  (vector-push-extend character (slot-value stream '%string)))

(defmethod tgs:stream-write-string ((stream %string-output-stream) string &optional start end)
  (let* ((%string (slot-value stream '%string))
         (len (- (or end (length string)) (or start 0)))
         (prev-len (fill-pointer %string))
         (available-space (- (array-dimension %string 0) prev-len))
         (new-len (+ prev-len len)))
    (when (< available-space len)
      (adjust-array %string new-len))
    (setf (fill-pointer %string) new-len)
    (replace %string string :start1 prev-len :start2 (or start 0) :end2 end)))

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

(defclass writer ()
  ((%stream :initarg :stream)
   (%coerce-key :initarg :coerce-key)
   (%pretty :initarg :pretty)
   (%stack :initform nil)
   (%ref-stack :initform nil)
   (%replacer :initarg :replacer)
   (%depth :type integer :initform 0)
   (%max-depth :initarg :max-depth)
   (%close-action :initarg :close-action))
  (:documentation "A JSON writer on which to call `write-value', `begin-object', etc.")
  (:default-initargs
   :stream (make-broadcast-stream)
   :coerce-key #'coerce-key
   :pretty nil
   :replacer nil
   :max-depth 128))

(defun make-writer (&key
                      (stream t)
                      (pretty nil)
                      (replacer nil)
                      coerce-key
                      (max-depth 128))
  "Create a writer for subsequent `write-value', `begin-object', et al calls.
  `:stream' like the `destination' in `format', or a `pathname'
  `:pretty' if true, pretty-format the output
  `:replacer' a function which takes a key and value as an argument, and returns t or nil, indicating whether the KV pair should be written.
    - Optionally returns a second value, indicating the value to be stringified in place of the given value.
  `:coerce-key' is a function of one argument, and is used to coerce object keys into non-nil string designators
  `:max-depth' is an integer denoting the maximum depth to allow nesting during writing, or nil

 see `coerce-key'"
  (check-type max-depth (or boolean (integer 1 #xFFFF)))
  (multiple-value-bind (stream close-action)
      (cond
        ((null stream) (values (make-broadcast-stream) (lambda ())))
        ((stringp stream)
          (unless (and (adjustable-array-p stream) (array-has-fill-pointer-p stream))
            (error 'type-error :datum stream :expected-type '(and string (adjustable-array-p stream) (satisfies array-has-fill-pointer-p))))

          (values (make-instance '%string-output-stream :string stream)
                 (lambda ())))
        ((pathnamep stream)
          (let ((stream (open stream :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede
                                     :external-format :utf-8)))
            (values stream (lambda () (close stream)))))
        ((streamp stream)
          (unless (output-stream-p stream)
            (error 'type-error :datum stream :expected-type '(and stream (satisfies output-stream-p))))
          (if (subtypep (stream-element-type stream) 'character)
            (values stream (lambda ()))
            (let ((stream (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
              (values stream (lambda () (close stream))))))
        ((eq stream t)
          (values *standard-output* (lambda ())))
        (t
          (error 'type-error :datum stream :expected-type '(or null
                                                               (and string (adjustable-array-p stream) (satisfies array-has-fill-pointer-p))
                                                               pathname
                                                               (and stream (satisfies output-stream-p))
                                                               (eql t)))))
    (make-instance 'writer :stream stream
                           :coerce-key (or coerce-key #'coerce-key)
                           :pretty (and pretty t)
                           :replacer replacer
                           :max-depth (case max-depth
                                        ((nil)  #xFFFF)
                                        ((t)    128)
                                        (t      max-depth))
                           :close-action close-action)))

(defun close-writer (writer)
  "Close the `writer'."
  (check-type writer writer)
  (let ((action (shiftf (slot-value writer '%close-action) nil)))
    (when action
      (funcall (the function action))))
  writer)

(defmacro with-writer ((var &rest args) &body body)
  "Create a `writer', ensuring `close-writer' is called on it on exit.

see `make-writer'
see `close-writer'"
  (let ((writer-sym (gensym (string '#:writer))))
    `(let ((,writer-sym (make-writer ,@args)))
      (unwind-protect (let ((,var ,writer-sym)) ,@body)
        (close-writer ,writer-sym)))))

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

(defun begin-object (writer)
  "Begin writing an object to `writer'.

see `write-key'
see `write-property'
see `write-properties'
see `end-object'"
  (check-type writer writer)
  (when (null (slot-value writer '%close-action))
    (error 'json-write-error :format-control "The writer has been closed."))
  (with-slots (%stream %stack %depth %max-depth) writer
    (case (car %stack)
      ((:array)                     (progn (%write-indentation writer)
                                           (setf (car %stack) :array-value)))
      ((:array-value)               (progn (write-char #\, %stream)
                                           (%write-indentation writer)))
      ((:object-key)                (setf (car %stack) :object-value))
      ((:object :object-value)      (error 'json-write-error :format-control "Expecting object key"))
      ((:complete)                  (error 'json-write-error :format-control "Attempting to write object when value already written to writer")))

    (when (= %depth %max-depth)
      (error 'json-write-limit-error :format-control "Exceeded maximum depth in writing object." :limit %max-depth))
    (incf %depth)
    (push :object %stack)
    (write-char #\{ %stream))
  writer)

(defun write-key (writer key)
  "Write an object `key' to `writer'. Must currently be writing an object.

see `begin-object'
see `with-object'"
  (check-type writer writer)
  (when (null (slot-value writer '%close-action))
    (error 'json-write-error :format-control "The writer has been closed."))
  (with-slots (%stream %coerce-key %stack %pretty) writer
    (let ((context (car %stack)))
      (case context
        (:object       (progn (setf (car %stack) :object-key)
                              (%write-indentation writer)))
        (:object-value (progn (write-char #\, %stream)
                              (%write-indentation writer)
                              (setf (car %stack) :object-key)))
        (t             (error 'json-write-error :format-control "Attempting to write object key while in ~A" :format-arguments context))))
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
  (check-type writer writer)
  (when (null (slot-value writer '%close-action))
    (error 'json-write-error :format-control "The writer has been closed."))
  (with-slots (%stream %stack %pretty %depth %max-depth) writer
    (let ((context (car %stack)))
      (case context
        ((:object :object-value))
        (:object-key (error 'json-write-error :format-control "Attempting to close object before writing key value"))
        (t           (error 'json-write-error :format-control "Attempting to close object while in ~A" :format-arguments context)))
      (pop %stack)
      (decf %depth)
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
  (check-type writer writer)
  (when (null (slot-value writer '%close-action))
    (error 'json-write-error :format-control "The writer has been closed."))
  (with-slots (%stream %stack %depth %max-depth) writer
    (case (car %stack)
      ((:array)                (progn (%write-indentation writer)
                                      (setf (car %stack) :array-value)))
      ((:array-value)          (progn (write-char #\, %stream)
                                      (%write-indentation writer)))
      ((:object-key)           (setf (car %stack) :object-value))
      ((:object :object-value) (error 'json-write-error :format-control "Expecting object key."))
      ((:complete)             (error 'json-write-error :format-control "Attempting to write array when value already written to writer.")))
    (push :array %stack)
    (when (= %depth %max-depth)
      (error 'json-write-limit-error :format-control "Exceeded maximum depth in writing array." :limit %max-depth))
    (incf %depth)
    (write-char #\[ %stream))
  writer)

(defun end-array (writer)
  "Finish writing an array to `writer'. Must match an opening `begin-array'."
  (check-type writer writer)
  (when (null (slot-value writer '%close-action))
    (error 'json-write-error :format-control "The writer has been closed."))
  (with-slots (%stream %stack %depth %max-depth) writer
    (let ((context (car %stack)))
      (case context
        ((:array :array-value))
        (t                     (error 'json-write-error :format-control "Attempting to close array while in ~A" context)))
      (pop %stack)
      (decf %depth)
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

(defun %write-json-atom (writer value)
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
      (double-float (sf:write-double value %stream))
      (string       (%write-json-string value %stream)))))

(declaim (inline %write-value-using-coerced-fields))
(defun %write-value-using-coerced-fields (writer value)
  (declare (type writer writer))
  (with-object writer
    (loop :for (name value . type-cell) :in (coerced-fields value)
          :for type := (if type-cell (car type-cell) t)
          :for coerced-value := (or value
                                    (cond ((%type= type 'boolean)  nil)
                                          ((%type= type 'list)     #())
                                          (t                       'null)))
          :do
            (write-key writer name)
            (write-value writer coerced-value))))

(defgeneric write-value (writer value)
  (:documentation "Write a JSON value to `writer'. Specialize this function for customized JSON writing.")
  (:method :around ((writer writer) value)
    (when (null (slot-value writer '%close-action))
      (error 'json-write-error :format-control "The writer has been closed."))
    (with-slots (%stack %ref-stack %pretty %replacer) writer
      (let ((context (car %stack)))
        (case context
          ((:object :object-value) (error 'json-write-error :format-control "Expecting object key"))
          ((:complete)             (error 'json-write-error :format-control "Attempting to write value when value already written to writer")))

        (let ((prev-stack %ref-stack))
          (let ((path (member value prev-stack :test #'eq)))
            (when path
              ;; bail with a ref string
              (error 'json-recursive-write-error :format-control "Recursion detected printing value" :path (reverse path))))
          (setf %ref-stack (cons value prev-stack))

          ;; Call the replacer on the top-level object first, if applicable

          (unwind-protect (progn
                            (if (and (null context) %replacer)
                              (multiple-value-call
                                  (lambda (write-p &optional (new-value value))
                                    (when write-p
                                      (if (eql value new-value)
                                        (call-next-method writer value)
                                        (call-next-method writer new-value))))
                                (funcall %replacer nil value))
                              (call-next-method writer value))
                            (case context
                              (:array      (setf (car %stack) :array-value))
                              (:object-key (setf (car %stack) :object-value))
                              ((nil)       (push :complete %stack))))
            (setf %ref-stack prev-stack)))))
    writer)

  ;;; `json-atom' specializations
  (:method ((writer writer) (value (eql 't)))
    (%write-json-atom writer value))
  (:method ((writer writer) (value (eql 'nil)))
    (%write-json-atom writer value))
  (:method ((writer writer) (value (eql 'null)))
    (%write-json-atom writer value))
  (:method ((writer writer) (value integer))
    (%write-json-atom writer value))
  (:method ((writer writer) (value float))
    (with-slots (%stream %stack) writer
      (case (car %stack)
        ((:array :object)
         (%write-indentation writer))
        ((:array-value)
         (write-char #\, %stream)
         (%write-indentation writer)))
      (macrolet ((#1=#:|| ()
                  `(etypecase value
                    (double-float (sf:write-double value %stream))
                    (single-float (sf:write-float value %stream))
                    ,@(unless (%type= 'short-float 'single-float)
                      '((short-float (sf:write-float (coerce value 'single-float) %stream))))
                    ,@(unless (%type= 'long-float 'double-float)
                      '((long-float (sf:write-double (coerce value 'double-float) %stream)))))))
        (#1#))))
  (:method ((writer writer) (value string))
    (%write-json-atom writer value))

  ;;; array
  (:method ((writer writer) (value vector))
    (with-array writer
      (let ((replacer (slot-value writer '%replacer)))
        (map nil
             (if replacer
               ;; Apply the replacer to each element in the sequence, with the index as its key
               (let ((i 0))
                 (lambda (x)
                   (multiple-value-call (lambda (write-p &optional (new-value nil value-changed-p))
                                          (when write-p
                                            (write-value writer (if value-changed-p new-value x))))
                     (funcall replacer i x))
                   (incf i)))
               (lambda (x)
                 (write-value writer x)))
              value))))

  ;; object
  (:method ((writer writer) (value hash-table))
    (with-object writer
      (maphash (let ((replacer (slot-value writer '%replacer)))
                 (if replacer
                   (lambda (key x)
                     (multiple-value-call (lambda (write-p &optional (new-value nil value-changed-p))
                                            (when write-p
                                              (write-key writer key)
                                              (write-value writer (if value-changed-p new-value x))))
                         (funcall replacer key x)))
                   (lambda (key x)
                     (write-key writer key)
                     (write-value writer x))))
             value)))

  ;;; Symbols
  (:method ((writer writer) (value symbol))
    (%write-json-atom writer (string value)))

  ;; Characters
  (:method ((writer writer) (value character))
    (%write-json-atom writer (string value)))

  ;;; Pathnames
  (:method  ((writer writer) (value pathname))
    (%write-json-atom writer (uiop:native-namestring value)))

  ;; Ratio
  (:method ((writer writer) (value ratio))
    (%write-json-atom writer (rtd:ratio-to-double value)))

  ;; Complex
  (:method ((writer writer) (value complex))
    (write-array writer (realpart value) (imagpart value)))

  ;; Multi-dimensional arrays
  (:method ((writer writer) (value array))
    (let ((dimensions (array-dimensions value)))
      (if (null dimensions)
          (write-value writer (aref value))
          (labels ((recurse (dimensions acc)
                     (destructuring-bind (d . rest) dimensions
                       (with-array writer
                         (if (null rest)
                           (loop :for i :below d
                                 :do (write-value writer (row-major-aref value (+ acc i))))
                           (loop :for i :below d
                                 :do (recurse rest (+ acc (* i d)))))))))
            (recurse dimensions 0)))))

  ;;; Sequence support
  (:method ((writer writer) (value sequence))
    (with-array writer
      (let ((replacer (slot-value writer '%replacer)))
        (map nil
             (if replacer
               ;; Apply the replacer to each element in the sequence, with the index as its key
               (let ((i 0))
                 (lambda (x)
                   (multiple-value-call (lambda (write-p &optional (new-value nil value-changed-p))
                                          (when write-p
                                            (write-value writer (if value-changed-p new-value x))))
                     (funcall replacer i x))
                   (incf i)))
               (lambda (x)
                 (write-value writer x)))
              value))))
  ;; standard-object
  (:method ((writer writer) (value standard-object))
    (%write-value-using-coerced-fields writer value))

  ;; structure-object
  #+ (or ccl clisp sbcl)
  (:method ((writer writer) (value structure-object))
    (%write-value-using-coerced-fields writer value)))

;;; Additional convenience functions/macros
(defun write-values (writer &rest values)
  "Convenience function to write multiple `values' to `writer'. Must be writing an array."
  (declare (dynamic-extent values))
  (check-type writer writer)
  (unless (or values (member (slot-value writer '%stack) '(:array :array-value)))
    (error 'json-write-error :format-control "Attempting to write multiple values outside of an array."))
  (map nil (lambda (x) (write-value writer x)) values)
  writer)

(defun write-array (writer &rest values)
  "Write an array from a series of `values.'"
  (with-array writer
    (apply #'write-values writer values)))

(defun write-property (writer key value)
  "Write an object property/key value pair."
  (check-type writer writer)
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
  (check-type writer writer)
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
  (check-type writer writer)
  (with-object writer
    (apply #'write-properties writer kvp)))

;; dynavar-based write functions
(defvar *writer*)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (documentation '*writer* 'variable) "The active `writer' for the various `write-*' functions."))

(defmacro with-writer* ((&rest args &key &allow-other-keys) &body body)
  "Create a new `writer' using `args' and bind it to `*writer*'

  `args' are a the same as `make-writer'"
  `(with-writer (*writer* ,@args)
     ,@body))

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

(defun stringify (element &key stream pretty replacer (coerce-key #'coerce-key))
  "Serialize `element' into JSON.
 Returns a fresh string if `stream' is nil, nil otherwise.
  `:stream' like the `destination' in `format', or a `pathname'
  `:pretty' if true, pretty-format the output
  `:replacer' a function which takes a key and value as an argument, and returns t or nil, indicating whether the KV pair should be written.
    - Optionally returns a second value, indicating the value to be stringified in place of the given value.
  `:coerce-key' is a function of one argument, and is used to coerce object keys into non-nil string designators
  `:max-depth' is an integer denoting the maximum depth to allow nesting during writing, or nil

 see `coerce-key'
"
  (check-type replacer (or symbol function))
  (when replacer
    (setf replacer (%ensure-function replacer)))
  (check-type coerce-key (or symbol function))
  (let ((coerce-key (%ensure-function coerce-key)))
    (flet ((stringify-to (stream)
             (with-writer (writer :stream stream
                                  :pretty (and pretty t)
                                  :replacer replacer
                                  :coerce-key coerce-key)
               (write-value writer element))))
      (cond
        ((null stream)
         (with-output-to-string (stream)
           (stringify-to stream)))
        (t
          (stringify-to stream)
          nil)))))
            

#| standard CL types
arithmetic-error
array
atom
base-char
base-string
bignum
bit
bit-vector
boolean
broadcast-stream
built-in-class
cell-error
character
class
compiled-function
complex
concatenated-stream
condition
cons
control-error
division-by-zero
double-float
echo-stream
end-of-file
error
extended-char
file-error
file-stream
fixnum
float
floating-point-inexact
floating-point-invalid-operation
floating-point-overflow
floating-point-underflow
function
generic-function
hash-table
integer
keyword
list
logical-pathname
long-float
method
method-combination
nil
null
number
package
package-error
parse-error
pathname
print-not-readable
program-error
random-state
ratio
rational
reader-error
readtable
real
restart
sequence
serious-condition
short-float
signed-byte
simple-array
simple-base-string
simple-bit-vector
simple-condition
simple-error
simple-string
simple-type-error
simple-vector
simple-warning
single-float
standard-char
standard-class
standard-generic-function
standard-method
standard-object
storage-condition
stream
stream-error
string
string-stream
structure-class
structure-object
style-warning
symbol
synonym-stream
t
two-way-stream
type-error
unbound-slot
unbound-variable
undefined-function
unsigned-byte
vector
warning
|#

#| Uncovered
arithmetic-error
broadcast-stream
built-in-class
cell-error
class
compiled-function
concatenated-stream
condition
control-error
division-by-zero
echo-stream
end-of-file
error
file-error
file-stream
floating-point-inexact
floating-point-invalid-operation
floating-point-overflow
floating-point-underflow
function
generic-function
method
method-combination
package-error
parse-error
print-not-readable
program-error
random-state
reader-error
readtable
restart
serious-condition
simple-condition
simple-error
simple-type-error
simple-warning
standard-char
standard-class
standard-generic-function
standard-method
standard-object
storage-condition
stream
stream-error
string-stream
structure-class
structure-object
style-warning
synonym-stream
two-way-stream
type-error
unbound-slot
unbound-variable
undefined-function
warning
|#


(defun %type-part-or (spec n default)
  (if (atom spec)
      default
      (let ((part (nthcdr n spec)))
        (if part
            (let ((part-value (car part)))
              (if (eq part-value '*)
                  default
                  part-value))
            default))))

(defun %convert (value type)
  (let* ((exp-type (ie:typexpand type))
         (type-name (if (atom exp-type) exp-type (car exp-type)))) 
    (cond
      ((eq t      type-name)        value)
      ((eq nil    type-name)        (error "Cannot convert '~A' to empty type. ~A" value type))
      ((eq type-name 'values)       (error "Cannot convert '~A' to type ~A." value type))
            
      ;; Boolean has special logic regarding things
      ;; like strings and numbers, so needs to go first
      ((%type= type 'boolean)
        (cond
          ((eq value nil)   nil)
          ((eq value t)     t)
          ((numberp value)  (not (zerop value)))
          ((stringp value)  (multiple-value-bind (parsed-value errorp) (parse value)
                              (when (or errorp (not (typep parsed-value 'boolean))) (error "Cannot coerce '~A' to type ~A." value type))
                              parsed-value))
          (t                      (error "Cannot coerce '~A' to type ~A." value type))))

      ((or (eq type-name 'eql)
           (eq type-name 'member))
        (dolist (eql-value (cdr exp-type) (error "~A can't be converted to type ~A" value type))
          (let ((coerced-value (typecase eql-value
                                 ;; w/ ignore-errors, we know it will be nil
                                 ;; in all cases where it is NOT a symbol
                                 ;; so we won't accidentlaly match the symbol nil
                                 (integer    (ignore-errors (%convert value 'integer)))
                                 (float      (ignore-errors (float (%convert value 'float) eql-value)))
                                 (character  (ignore-errors (%convert value 'character)))
                                 (symbol
                                   (if (typecase value
                                         (string    (string= eql-value value))
                                         (json-atom (string= eql-value (stringify value))))
                                     eql-value
                                     ;; we know it's a symbol so it won't be eql to 0
                                     0)))))
            (when (eql eql-value coerced-value)
              (return coerced-value)))))
      ;; For these, either the value is already of that type,
      ;; or we can't do anything about it
      ((or (eq type-name 'atom)
           (eq type-name 'not)
           (eq type-name 'satisfies))
        (unless (typep value type) (error "~A can't be converted to type ~A" value type))
        value)
      ((eq type-name 'and)
        (let* ((types (stable-sort (copy-list (rest exp-type)) #'subtypep))
              (main-type (first types))
              (coerced (%convert value main-type))
              (no-fits (remove coerced (rest types) :test #'typep)))
          (when no-fits
            (error "Coercing 'and' - Value '~A' coerced to '~A' via '~A' fails constraint for ~A" value coerced main-type no-fits))
          (values coerced no-fits)))
      ((eq type-name 'or)
        ;; TODO - Should we try and be nice and order the types for them?
        ;; there's no absolute ordering of types, but at least within some group
        ;; we can try and be helpful (eg if we are given (or number float ladder)
        ;; we can try for float before number, and UB with the order of ladder
        ;; or maybe it's best to leave it in the order it comes in as the user can control it
        (let ((types (rest exp-type)))
          (dolist (type types (error "Coercing 'or' - '~A' is not any of ~{~A~^, ~}" value types))
            (multiple-value-bind (value errorp) (ignore-errors (%convert value type))
              (unless errorp
                (return value))))))



      ;; 'Native' JSON types (except for boolean above)
      ((eq 'null  type-name)        (cond
                                      ((eq value 'null)       nil)
                                      (t                      (error "Cannot coerce '~A' to ~A" value type))))
      ((subtypep type 'number)
        (typecase value
          (string (multiple-value-bind (parsed-value errorp) (parse value)
                    (when (or errorp (not (numberp parsed-value))) (error "Cannot coerce '~A' to type ~A." value type))
                    
                    (coerce parsed-value type)))
          (number (coerce value type))
          (vector
            (unless (and (eq type-name 'complex)
                         (= (length value) 2))
              (error "Cannot coerce '~A' into type ~A." value type))
            (let ((realpart (%convert (aref value 0) 'real))
                  (imagpart (%convert (aref value 1) 'real)))
              (complex realpart imagpart)))
          (t  (error "Cannot coerce '~A' into type ~A." value type))))
      ((subtypep type 'string)
        (let ((value (typecase value
                       (string    value)
                       (json-atom (stringify value))
                       (t (error "Cannot coerce '~A' to type ~A." value type)))))
          (coerce value type)))
      ((eq type-name 'simple-vector)
        (unless (vectorp value)
          (error "Cannot coerce '~A' into type ~A" value type))
        ;; need to make sure len matches
        (coerce value type))
      ((eq type-name 'hash-table)
        (unless (hash-table-p value)
          (error "Cannot coerce '~A' into type ~A" value type))
        value)

      ;; Note, we handle strings separately because
      ;; for strings we generally are able to take advantage
      ;; of raw string values or stringify for atoms
      ;; for non-string arrays however, we don't have any such rules
      ((member type-name '(array
                            simple-array
                            bit-vector
                            simple-bit-vector
                            vector))
        (multiple-value-bind (elt-type dimensions)
          (multiple-value-bind (elt-type dimensions)
              (let ((normalized (if (atom exp-type) (list exp-type) exp-type)))
                (ecase type-name
                  ((array simple-array)
                   ;; Note - handle nil typed array (eg array that can hold nothing)
                   (values (%type-part-or normalized 1 't)
                           (let ((dimension-spec (%type-part-or normalized 2 '*)))
                             (if (integerp dimension-spec)
                                 (make-list dimension-spec :initial-element '*)
                                 dimension-spec))))
                  ((bit-vector simple-bit-vector)
                   (values 'bit (list (or (second normalized) '*))))
                  ((vector)
                   (values (%type-part-or normalized 1 't) (list (%type-part-or normalized 2 '*))))
                  ((simple-vector)
                   (values 't (list (or (second normalized) '*))))))
            (values elt-type (if (listp dimensions) dimensions (list dimensions))))

          (let* ((initial-contents
                  (labels ((recurse (dimensions x)
                            (cond
                              ((null dimensions) (%convert x elt-type))
                              (t
                                (unless (typep x 'sequence) (error "Cannot coerce '~A' into type ~A" value type))
                                (let* ((dimension (car dimensions))
                                       (dimension (if (eq dimension '*) (length x) dimension)))
                                  (unless (or (integerp dimension) (eq '* dimension))
                                    (error "bad dimension in array type: ~A" dimension))
                                  (map (list 'simple-vector dimension) (lambda (x) (recurse (cdr dimensions) x)) x))))))
                      (recurse dimensions value)))
                 (size
                   (labels ((recurse (dimensions x)
                             (cond
                               ((null dimensions) nil)
                               (t
                                 (let ((rest (cdr dimensions)))
                                   (cons (length x)
                                         (and rest (recurse rest (aref x 0)))))))))
                     (recurse dimensions initial-contents))))
           
           (make-array size :element-type elt-type :initial-contents initial-contents))))
      ((subtypep type 'character)
        (coerce
          (typecase value
            (string    value)
            (json-atom (stringify value))
            (t         (error "Cannot coerce '~A' into type ~A" value type)))
          type))
      ((subtypep type 'sequence)
        (unless (vectorp value)
          (error "Cannot coerce '~A' into type ~A" value type))
        (coerce value type))
      ((eq type-name 'package)
        (let ((name (typecase value
                     (string    value)
                     (json-atom (stringify value))
                     (t         (error "Cannot coerce '~A' into type ~A" value type)))))
         (or (find-package name)
             (error "Cannot coerce '~A' into type ~A (package does not exist)." value type))))
      ((eq type-name 'pathname)
       (values (parse-namestring (typecase value
                                   (string    value)
                                   (json-atom (stringify value))
                                   (t         (error "Cannot coerce '~A' into type ~A" value type))))))
      ((eq type-name 'keyword)
       (let ((name (typecase value
                     (string    value)
                     (json-atom (stringify value))
                     (t         (error "Cannot coerce '~A' into type ~A" value type)))))
         (intern name '#:keyword)))
      ((eq type-name 'symbol)
       (let ((name (typecase value
                     (string    value)
                     (json-atom (stringify value))
                     (t         (error "Cannot coerce '~A' into type ~A" value type)))))
         (intern name)))
      (t
       (let ((class (find-class type nil)))
         (unless class (error "Cannot coerce '~A' to '~A'." value type))
         (unless (hash-table-p value) (error "Cannot coerce '~A' to '~A'." value type))
         (let (;; plist of :initarg form slot/values for `make-instance'
               (initarg-slots-plist ())
               ;; alist of (slotd . value) for `(setf cmop:slot-value-using-class)'
               (set-value-slots-alist ()))
           (declare (dynamic-extent initarg-slots-plist set-value-slots-alist))
           (c2mop:ensure-finalized class)
           (dolist (slot (c2mop:class-slots class))
             (multiple-value-bind (value value-p) (gethash (coerce-key (c2mop:slot-definition-name slot)) value)
               (when value-p
                 (let ((coerce-value (%convert value (c2cl:slot-definition-type slot)))
                       (slot-initargs (c2mop:slot-definition-initargs slot)))
                   (cond
                     (slot-initargs
                      (push coerce-value initarg-slots-plist)
                      (push (first slot-initargs) initarg-slots-plist))
                     (t
                      (push (cons slot coerce-value) set-value-slots-alist)))))))
           (let ((instance (apply #'make-instance class initarg-slots-plist)))
             (loop :for (slot . value) :in set-value-slots-alist
                   :do (setf (c2mop:slot-value-using-class class instance slot) value))
             instance)))))))

(defun convert (json type)
  "Convert the JSON `json' to `type'.
  
  `json' may be any valid input to `parse`."
  (check-type json string)
  (%convert (parse json) type))
