(defpackage #:com.inuoe.jzon-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:hash-table-keys
   #:plist-hash-table)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:is-every
   #:signals
   #:test)
  (:import-from #:flexi-streams)
  (:import-from #:uiop)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:fs #:flexi-streams))
  (:export
   #:jzon
   #:run
   #:main))

(in-package #:com.inuoe.jzon-tests)

(def-suite jzon
  :description "Tests for the jzon library.")

(defun run ()
  (fiveam:run! 'jzon))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run)))
    (if result 0 -1)))

(in-suite jzon)

(def-suite parsing :in jzon)

(in-suite parsing)

(defun ph (&rest plist)
  "Shorthand for plist-hash-table."
  (plist-hash-table plist :test 'equal))

(defun utf-8 (string)
  (fs:string-to-octets string :external-format :utf-8))

(test parses-atoms
  (is (eq 'null (jzon:parse "null")))
  (is (eq 'null (jzon:parse "  null")))
  (is (eq 't (jzon:parse "true")))
  (is (eq 't (jzon:parse "  true")))
  (is (eq 'nil (jzon:parse "false")))
  (is (eq 'nil (jzon:parse "  false"))))

(test parses-atoms-error-on-incomplete
  (signals (jzon:json-parse-error)
    (jzon:parse "nul   "))
  (signals (jzon:json-parse-error)
    (jzon:parse "nu    "))
  (signals (jzon:json-parse-error)
    (jzon:parse "n     "))
  (signals (jzon:json-parse-error)
    (jzon:parse "nul"))
  (signals (jzon:json-parse-error)
    (jzon:parse "nu"))
  (signals (jzon:json-parse-error)
    (jzon:parse "n"))
  (signals (jzon:json-parse-error)
    (jzon:parse "tru    "))
  (signals (jzon:json-parse-error)
    (jzon:parse "tr     "))
  (signals (jzon:json-parse-error)
    (jzon:parse "t      "))
  (signals (jzon:json-parse-error)
    (jzon:parse "tru"))
  (signals (jzon:json-parse-error)
    (jzon:parse "tr"))
  (signals (jzon:json-parse-error)
    (jzon:parse "t"))
  (signals (jzon:json-parse-error)
    (jzon:parse "fals   "))
  (signals (jzon:json-parse-error)
    (jzon:parse "fal    "))
  (signals (jzon:json-parse-error)
    (jzon:parse "fa     "))
  (signals (jzon:json-parse-error)
    (jzon:parse "f      "))
  (signals (jzon:json-parse-error)
    (jzon:parse "fals"))
  (signals (jzon:json-parse-error)
    (jzon:parse "fal"))
  (signals (jzon:json-parse-error)
    (jzon:parse "fa"))
  (signals (jzon:json-parse-error)
    (jzon:parse "f")))

(test parses-integers
  (is (integerp (jzon:parse "42")))
  (is (= 42 (jzon:parse "42"))))

(test parses-decimals
  (is (typep (jzon:parse "42.0") 'double-float))
  (is (= 42.0d0 (jzon:parse "42.0"))))

(test parses-exponent
  (is (typep (jzon:parse "42e1") 'double-float))
  (is (= 420.0d0 (jzon:parse "42e1"))))

(test parses-decimals-with-exponent
  (is (= 42.0d0 (jzon:parse "0.42e2"))))

(test parse-negative-decimal
  (is (= -0.1d0 (jzon:parse "-0.1"))))

(test disallows-leading-zeros
  (signals (jzon:json-parse-error)
    (jzon:parse "01"))
  (signals (jzon:json-parse-error)
    (jzon:parse "01.0"))
  (signals (jzon:json-parse-error)
    (jzon:parse "01e10")))

(test disallows-trailing-decimal-point
  (signals (jzon:json-parse-error)
    (jzon:parse "1."))
  (signals (jzon:json-parse-error)
    (jzon:parse "1.e10")))

(test disallows-trailing-exponent-marker
  (signals (jzon:json-parse-error)
    (jzon:parse "1e"))
  (signals (jzon:json-parse-error)
    (jzon:parse "1.0e"))
  (signals (jzon:json-parse-error)
    (jzon:parse "0e")))

(test parses-negative-zero.0
  (is (= -0.0d0 (jzon:parse "-0.0"))))

(test parses-negative-zero
  (is (= -0.0d0 (jzon:parse "-0"))))

(test parses-arrays
  (is (equalp #() (jzon:parse "[]")))
  (is (equalp #(1 2 3) (jzon:parse "[1, 2, 3]"))))

(test parses-arrays-signals-eof
  (signals (jzon:json-eof-error)
    (jzon:parse "[1, 2, 3")))

(test parses-arrays-disallows-trailing-comma
  (signals (jzon:json-parse-error)
    (jzon:parse "[1, 2, 3,]")))

(test parses-arrays-allows-trailing-comma-when-asked
  (is (equalp #(1 2 3) (jzon:parse "[1, 2, 3,]" :allow-trailing-comma t))))

(test parses-arrays-disallows-several-trailing-commas
  (signals (jzon:json-parse-error)
    (jzon:parse "[1, 2, 3,,]"))
  (signals (jzon:json-parse-error)
    (jzon:parse "[1, 2, 3,,]" :allow-trailing-comma t)))

(test parses-arrays-disallows-empty-with-comma
  (signals (jzon:json-parse-error)
    (jzon:parse "[,]"))
  (signals (jzon:json-parse-error)
    (jzon:parse "[,]" :allow-trailing-comma t)))

(test parses-arrays-disallows-trailing-comma-with-eof
  (signals (jzon:json-eof-error)
    (jzon:parse "[1, 2, 3,"))
  (signals (jzon:json-eof-error)
    (jzon:parse "[1, 2, 3," :allow-trailing-comma t)))

(test parses-objects
  (is (equalp (ph) (jzon:parse "{}")))
  (is (equalp (ph "x" 1 "y" 2) (jzon:parse "{\"x\": 1, \"y\": 2}"))))

(test parses-objects-eof
  (signals (jzon:json-eof-error)
    (jzon:parse "{"))
  (signals (jzon:json-eof-error)
    (jzon:parse "{\"x\": 1, \"y\": 2")))

(test parses-objects-disallows-trailing-comma
  (signals (jzon:json-parse-error)
    (jzon:parse "{\"x\": 1, \"y\": 2,}")))

(test parses-objects-allows-trailing-comma-when-asked
  (is (equalp (ph "x" 1 "y" 2) (jzon:parse "{\"x\": 1, \"y\": 2,}" :allow-trailing-comma t))))

(test parses-objects-disallows-several-trailing-commas
  (signals (jzon:json-parse-error)
    (jzon:parse "{\"x\": 1, \"y\": 2,,}"))
  (signals (jzon:json-parse-error)
    (jzon:parse "{\"x\": 1, \"y\": 2,,}" :allow-trailing-comma t)))

(test parses-object-disallows-trailing-comma-with-eof
  (signals (jzon:json-eof-error)
    (jzon:parse "{\"x\": 1, \"y\": 2,"))
  (signals (jzon:json-eof-error)
    (jzon:parse "{\"x\": 1, \"y\": 2," :allow-trailing-comma t)))

(test parses-object-disallows-empty-with-comma
  (signals (jzon:json-parse-error)
    (jzon:parse "{,}"))
  (signals (jzon:json-parse-error)
    (jzon:parse "{,}" :allow-trailing-comma t)))

(test parse-singular
  (is (equalp (ph "foo" "bar")
              (jzon:parse "{\"foo\":\"bar\"}"))
      "Matching of a single simple string")
  (is (equalp (ph "bar" 1000)
              (jzon:parse "{\"bar\":1000}"))
      "Matching of a single number")
  (is (equalp (ph "bar" 10.1d0)
              (jzon:parse "{\"bar\":10.1}"))
      "Matching of a single decimal number")
  (is (equalp (ph "bar" #("foo" 10 101.1d0))
              (jzon:parse "{\"bar\":[\"foo\",10,101.10]}"))
      "Matching of an array with various types of elements"))

(test parse-multiple
  (is (equalp (ph "foo" "bar" "baz" "bang" "bing" 100 "bingo" 1.1d0 "bazo" #(1 2 "foo"))
              (jzon:parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":100,\"bingo\":1.1,\"bazo\":[1,2,\"foo\"]}"))
      "Parsing of multiple items of all kinds"))

(test parse-nested
  (is (equalp (ph "foo" (ph "bar" "baz"))
              (jzon:parse "{\"foo\":{\"bar\":\"baz\"}}"))
      "One object in one object")
  (is (equalp (ph "foo" "bar" "bie" (ph "bar" "baz" "bang" 1000) "bing" "bingo")
              (jzon:parse "{\"foo\":\"bar\",\"bie\":{\"bar\":\"baz\",\"bang\":1000},\"bing\":\"bingo\"}")))
  (is (equalp (ph "foo" (vector (ph "foo" "bar" "baz" 1000)))
              (jzon:parse "{\"foo\":[{\"foo\":\"bar\",\"baz\":1000}]}"))
      "Object in an array")
  (is (equalp (ph  "foo" "bar" "baz" (ph "boo" 100.1d0))
              (jzon:parse "{\"foo\":\"bar\",\"baz\":{\"boo\":100.10}}"))
      "Decimal number in inner object"))

(test unicode-chars
  (is (equalp (ph "λlambda" "💩poop")
              (jzon:parse "{\"\\u03BBlambda\":\"\\ud83d\\udca9poop\"}")))

  (is (equalp (ph "lambdaλ" "poop💩")
              (jzon:parse "{\"lambda\\u03BB\":\"poop\\ud83d\\udca9\"}")))

  (is (equalp (ph "lambdaλlambda" "poop💩poop")
              (jzon:parse "{\"lambda\\u03BBlambda\":\"poop\\ud83d\\udca9poop\"}"))))

(test parse-pools-keys
  (let* ((objects (jzon:parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]"))
         (keys (map 'list #'hash-table-keys objects))
         (xs (mapcar #'first keys))
         (x1 (first xs))
         (x2 (second xs))
         (x3 (third xs)))
    (is (eq x1 x2))
    (is (eq x2 x3))))

(test parse-uses-custom-key-fn
  (let ((keys ()))
    (jzon:parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn (lambda (key) (push key keys) key))
    (is (equalp '("x" "x" "x") keys))))

(test parse-ignores-pre-post-whitespace
  (is-every equalp
    (nil               (jzon:parse "  false "))
    (t                 (jzon:parse " true  "))
    ('null             (jzon:parse "   null "))
    (42                (jzon:parse "      42 "))
    (42.0d0            (jzon:parse "  42e0  "))
    ("Hello, world!"   (jzon:parse "   \"Hello, world!\"  "))
    (#(1 2 3)          (jzon:parse " [1,2,3]  "))
    ((ph "x" 10 "y" 0) (jzon:parse "   { \"x\": 10, \"y\": 0}   "))))

(test parse-accepts-stream
  (flet ((jzon:parse (str)
           (with-input-from-string (in str)
             (jzon:parse in))))
    (is-every equalp
      (nil               (jzon:parse "false"))
      (t                 (jzon:parse "true"))
      ('null             (jzon:parse "null"))
      (42                (jzon:parse "42"))
      (42.0d0            (jzon:parse "42e0"))
      ("Hello, world!"   (jzon:parse "\"Hello, world!\""))
      (#(1 2 3)          (jzon:parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (jzon:parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-accepts-pathname
  (flet ((jzon:parse (str)
           (uiop:with-temporary-file (:stream stream :pathname p :external-format :utf-8)
             (write-string str stream)
             (finish-output stream)
             (close stream)
             (jzon:parse p))))
    (is-every equalp
      (nil               (jzon:parse "false"))
      (t                 (jzon:parse "true"))
      ('null             (jzon:parse "null"))
      (42                (jzon:parse "42"))
      (42.0d0            (jzon:parse "42e0"))
      ("Hello, world!"   (jzon:parse "\"Hello, world!\""))
      (#(1 2 3)          (jzon:parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (jzon:parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-accepts-non-simple-string
  (flet ((jzon:parse (str)
           (jzon:parse (make-array (length str) :element-type 'character :fill-pointer t :initial-contents str))))
    (is-every equalp
      (nil               (jzon:parse "false"))
      (t                 (jzon:parse "true"))
      ('null             (jzon:parse "null"))
      (42                (jzon:parse "42"))
      (42.0d0            (jzon:parse "42e0"))
      ("Hello, world!"   (jzon:parse "\"Hello, world!\""))
      (#(1 2 3)          (jzon:parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (jzon:parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-returns-base-strings
  (is (eq '#.(upgraded-array-element-type 'base-char) (array-element-type (jzon:parse "\"COMMON-LISP\""))))
  (is (eq '#.(upgraded-array-element-type 'base-char) (array-element-type (jzon:parse "\"\\u0043\\u004F\\u004D\\u004D\\u004F\\u004E\\u002D\\u004C\\u0049\\u0053\\u0050\"")))))

(test parse-accepts-octet-vector
  (is-every equalp
    (nil               (jzon:parse (utf-8 "false")))
    (t                 (jzon:parse (utf-8 "true")))
    ('null             (jzon:parse (utf-8 "null")))
    (42                (jzon:parse (utf-8 "42")))
    (42.0d0            (jzon:parse (utf-8 "42e0")))
    ("Hello, world!"   (jzon:parse (utf-8 "\"Hello, world!\"")))
    (#(1 2 3)          (jzon:parse (utf-8 "[1,2,3]")))
    ((ph "x" 10 "y" 0) (jzon:parse (utf-8 "{ \"x\": 10, \"y\": 0}")))))


(test parse-accepts-binary-stream
  (flet ((jzon:parse (str)
           (jzon:parse (fs:make-in-memory-input-stream (fs:string-to-octets str)))))
    (is-every equalp
      (nil               (jzon:parse "false"))
      (t                 (jzon:parse "true"))
      ('null             (jzon:parse "null"))
      (42                (jzon:parse "42"))
      (42.0d0            (jzon:parse "42e0"))
      ("Hello, world!"   (jzon:parse "\"Hello, world!\""))
      (#(1 2 3)          (jzon:parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (jzon:parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-allows-strings-below-max-string-length
  (finishes (jzon:parse "\"This is a string that is not long\"" :max-string-length 45)))

(test parse-allows-strings-at-max-string-length
  (finishes (jzon:parse "\"This is a string that is exactly not too long\"" :max-string-length 45)))

(test parse-limits-max-string-length
  (signals (jzon:json-parse-error)
    (jzon:parse "\"This is a string that is too long\"" :max-string-length 5)))

(test parse-limits-max-string-length-with-escape-codes
  (signals (jzon:json-parse-error)
    (jzon:parse "\"This is a string that is too long\bwith some special codes \\u00f8\"" :max-string-length 5)))

(test parse-max-string-length-respects-escape-codes-1
  (finishes (jzon:parse "\"\\u00f8\"" :max-string-length 1)))

(test parse-max-string-length-respects-escape-codes-2
  (finishes (jzon:parse "\"\\n\"" :max-string-length 1)))

(test parse-errors-on-too-large-string-length
  (signals (type-error)
    (jzon:parse "\"Doesn't matter\"" :max-string-length (* array-dimension-limit 2))))

(test parse-disallows-comments
  (signals (jzon:json-parse-error)
    (jzon:parse "//Line comment
    123")))

(test parse-allows-comments-when-asked
  (is (= 123 (jzon:parse "//Line comment
  123" :allow-comments t))))

(test parse-line-comments-do-not-end-on-cr-only-lf
  (is (= 123 (jzon:parse (format nil "//Comment~C123~C 123" #\Return #\Linefeed) :allow-comments t)))
  (signals (jzon:json-eof-error)
    (jzon:parse (format nil "//Comment~C123 123" #\Return) :allow-comments t)))

(test parse-comments-delimit-atoms
  (is (= 123 (jzon:parse "123//Line comment" :allow-comments t)))
  (is (eq t (jzon:parse "true//Line comment" :allow-comments t)))
  (is (eq nil (jzon:parse "false//Line comment" :allow-comments t)))
  (is (eq 'null (jzon:parse "null//Line comment" :allow-comments t)))
  (is (string= "123" (jzon:parse "\"123\"//Line comment" :allow-comments t))))

(test parse-disallows-block-comments
  (signals (jzon:json-parse-error)
    (jzon:parse "/*Block comment*/ 123")))

(test parse-allows-block-comments-when-asked
  (is (= 123 (jzon:parse "/*Block comment*/ 123" :allow-comments t))))

(test parse-does-not-nest-block-comments
  (signals (jzon:json-parse-error)
    (jzon:parse "/*Block comment /*Nested Block Comment */ */ 123" :allow-comments t)))

(test unterminated-block-comment-errors
  (signals (jzon:json-eof-error)
    (jzon:parse "/* Some stuff" :allow-comments t))
  (signals (jzon:json-eof-error)
    (jzon:parse "/* Some stuff ** // " :allow-comments t)))

(test miscellaneous-block-comment-tests
  (is (= 123 (jzon:parse "123/*comment*/" :allow-comments t)))
  (is (eq t (jzon:parse "true/*comment*/" :allow-comments t)))
  (is (eq nil (jzon:parse "false/*comment*/" :allow-comments t)))
  (is (eq 'null (jzon:parse "null/*comment*/" :allow-comments t)))
  (is (string= "123" (jzon:parse "/*comment before */\"123\"/*comment*/" :allow-comments t)))
  (is (= 123 (jzon:parse "/*comment before */123/*comment*/" :allow-comments t)))
  (is (eq t (jzon:parse "/*comment before */true/*comment*/" :allow-comments t)))
  (is (eq nil (jzon:parse "/*comment before */false/*comment*/" :allow-comments t)))
  (is (eq 'null (jzon:parse "/*comment before */null/*comment*/" :allow-comments t)))
  (is (string= "123" (jzon:parse "/*comment before */\"123\"/*comment*/" :allow-comments t)))
  (is (= 123 (jzon:parse "/*comment before //line comment ignored inside block */123/*comment*/" :allow-comments t))))

(def-suite incremental :in parsing)
(in-suite incremental)

(test parse-next-basics
  (jzon:with-parser (parser "{\"x\": 42, \"y\": [1, 2, 3], \"z\": [true, false, null]}")
    (is (eq :begin-object (jzon:parse-next parser)))
    (is (eq :object-key (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :object-key (jzon:parse-next parser)))
    (is (eq :begin-array (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :end-array (jzon:parse-next parser)))
    (is (eq :object-key (jzon:parse-next parser)))
    (is (eq :begin-array (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :value (jzon:parse-next parser)))
    (is (eq :end-array (jzon:parse-next parser)))
    (is (eq :end-object (jzon:parse-next parser)))))

(test parse-next-errors-after-toplevel
  (jzon:with-parser (parser "42 24")
    (is (eq :value (jzon:parse-next parser)))
    (signals (jzon:json-parse-error)
      (jzon:parse-next parser))))

(test parse-next-after-toplevel-continues-failing
  (jzon:with-parser (parser "{} {")
    (is (eq :begin-object (jzon:parse-next parser)))
    (is (eq :end-object (jzon:parse-next parser)))
    (signals (jzon:json-parse-error)
      (jzon:parse-next parser))
    (signals (jzon:json-parse-error)
      (jzon:parse-next parser))))

(test multi-close-ok
  (jzon:with-parser (parser "{}")
    (jzon:close-parser parser)
    (jzon:close-parser parser)))

(test parse-next-after-complete-returns-nil
  (jzon:with-parser (parser "42")
    (is (eq :value (jzon:parse-next parser)))
    (is (eq nil (jzon:parse-next parser)))
    (is (eq nil (jzon:parse-next parser)))
    (is (eq nil (jzon:parse-next parser)))
    (is (eq nil (jzon:parse-next parser)))
    (is (eq nil (jzon:parse-next parser)))
    (is (eq nil (jzon:parse-next parser)))))

(test parse-next-after-close-errors
  (signals (jzon:json-error)
    (jzon:with-parser (parser "{}")
      (jzon:close-parser parser)
      (jzon:parse-next parser))))

(def-suite writer :in jzon)
(in-suite writer)

(defmacro with-writer-to-string ((writer &key pretty max-depth) &body body)
  (let ((str-sym (gensym "STR")))
    `(with-output-to-string (,str-sym)
       (let ((,writer (jzon:make-writer :stream ,str-sym :pretty ,pretty :max-depth ,max-depth)))
         ,@body))))

(test writer-write-values-works
  (is (string= "[1,2,3]" (with-writer-to-string (writer)
                           (jzon:with-array writer
                             (jzon:write-values writer 1 2 3))))))

(test writer-max-depth-works
  (signals (error)
    (with-writer-to-string (writer :max-depth 1)
      (jzon:with-array writer
        (jzon:with-array writer))))
  (finishes
    (with-writer-to-string (writer :max-depth 1)
      (jzon:with-array writer
        (jzon:write-value writer 42)))))

(test writer-disallows-more-than-one-toplevel-value
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:write-value 42)
      (jzon:write-value 24)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:with-object writer)
      (jzon:write-value writer 42)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:write-value writer 42)
      (jzon:with-object writer)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:with-array writer)
      (jzon:write-value writer 42)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:write-value writer 42)
      (jzon:with-array writer)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:with-array writer)
      (jzon:with-array writer)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:with-array writer)
      (jzon:with-object writer)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:with-object writer)
      (jzon:with-array writer)))
  (signals (error)
    (with-writer-to-string (writer)
      (jzon:with-object writer)
      (jzon:with-object writer))))

(test write-properties-returns-writer
  (let ((writer (jzon:make-writer)))
    (jzon:with-object writer
      (is (eq writer (jzon:write-properties writer 0 0))))))

(test write-array-works
  (is (string= "[1,2,3]"
       (with-writer-to-string (writer)
         (jzon:write-array writer 1 2 3)))))

(test write-*-functions-use-bound-writer
  (is (string= "42" (with-writer-to-string (jzon:*writer*) (jzon:write-value* 42))))
  (is (string= "[42]" (with-writer-to-string (jzon:*writer*) (jzon:write-array* 42))))
  (is (string= "{\"24\":42}" (with-writer-to-string (jzon:*writer*) (jzon:write-object* 24 42))))
  (is (string= "[42]" (with-writer-to-string (jzon:*writer*) (jzon:with-array* (jzon:write-value* 42)))))
  (is (string= "{\"24\":42}"
               (with-writer-to-string (jzon:*writer*)
                 (jzon:with-object*
                   (jzon:write-key* "24")
                   (jzon:write-value* 42)))))
  (is (string= "{\"24\":42}"
               (with-writer-to-string (jzon:*writer*)
                 (jzon:with-object*
                   (jzon:write-property* "24" 42)))))
  (is (string= "{\"24\":42,\"null\":null}"
               (with-writer-to-string (jzon:*writer*)
                 (jzon:with-object*
                   (jzon:write-properties* "24" 42
                                           "null" 'null))))))

(test with-writer-*-binds-writer
  (is (string= "\"hello\"" (with-output-to-string (stream)
                             (jzon:with-writer* (:stream stream)
                               (jzon:write-value* "hello"))))))

(def-suite stringify :in jzon)

(in-suite stringify)

(test stringify-to-nil-returns-string
  (is (string= "42" (jzon:stringify 42))))

(test stringify-to-t-writes-to-stdout
  (is (string= "42" (with-output-to-string (*standard-output*)
                      (jzon:stringify 42 :stream t)))))

(test stringify-to-stream-writes-to-stream
  (is (string= "42" (with-output-to-string (stream)
                      (jzon:stringify 42 :stream stream)))))

(test stringify-works-on-binary-streams
  (is (string= "42" (let ((stream (fs:make-in-memory-output-stream)))
                      (jzon:stringify 42 :stream stream)
                      (fs:octets-to-string (fs:get-output-stream-sequence stream))))))

(test stringify-pretty-array-spaces-elements
  (is (string= "[
  1,
  2,
  3
]" (jzon:stringify #(1 2 3) :pretty t))))

(test stringify-pretty-object-spaces-kv
  (is (string= "{
  \"x\": 0
}" (jzon:stringify (ph "x" 0) :pretty t))))

(test stringify-pretty-object-newlines-multiple-kv
  (is (string= "{
  \"x\": 0,
  \"y\": 5
}" (jzon:stringify (ph "x" 0 "y" 5) :pretty t))))

(test stringify-pretty-object-newlines-if-nested-object
  (is (string= "{
  \"obj\": {
    \"x\": 0,
    \"y\": 5
  }
}" (jzon:stringify (ph "obj" (ph "x" 0 "y" 5)) :pretty t))))

(test stringify-pretty-array-newlines-if-nested-object
  (is (string= "[
  1,
  {
    \"x\": 0,
    \"y\": 5
  }
]" (jzon:stringify (vector 1 (ph "x" 0 "y" 5)) :pretty t))))

(test string-expands-special-escapes
  (is-every string=
    (#\Backspace (jzon:parse "\"\\b\""))
    (#\Page  (jzon:parse "\"\\f\""))
    (#\Linefeed  (jzon:parse "\"\\n\""))
    (#\Return    (jzon:parse "\"\\r\""))
    (#\Tab       (jzon:parse "\"\\t\""))))

(test stringify-atoms
  (is-every string=
    ("true" (jzon:stringify t))
    ("false" (jzon:stringify nil))
    ("null" (jzon:stringify 'null))))

(test stringify-integers
  (is (string= "5" (jzon:stringify 5)))
  (is (string= "0" (jzon:stringify 0))))

(test stringify-strings
  (is (string= "\"hello, world!\"" (jzon:stringify "hello, world!")))
  (is (string= "\"\"" (jzon:stringify ""))))

(test stringify-string-handles-special-escapes
  (is-every string=
    ("\"\\b\"" (jzon:stringify (string #\Backspace)))
    ("\"\\f\"" (jzon:stringify (string #\Page)))
    ("\"\\n\"" (jzon:stringify (string #\Linefeed)))
    ("\"\\r\"" (jzon:stringify (string #\Return)))
    ("\"\\t\"" (jzon:stringify (string #\Tab)))))

(test stringify-array
  (is (string= "[]" (jzon:stringify #())))
  (is (string= "[42,\"hello\",[]]" (jzon:stringify #(42 "hello" #())))))

(defun recode (value)
  "Shorthand for (jzon:parse (jzon:stringify value))"
  (jzon:parse (jzon:stringify value)))

(test stringify-object
  (is (string= "{}" (jzon:stringify (ph))))
  ;; Note - We can't reliably test object string output because hash tables ordering might differ
  ;; So instead, parse and verify structure matches
  (is (equalp (ph "x" 100 "y" 45 "name" "Rock") (recode (ph "x" 100 "y" 45 "name" "Rock")))))

(defclass test-class ()
  ((a
    :initarg :a
    :type t)
   (b
    :initarg :b
    :type list)
   (c
    :initarg :c
    :type boolean)))

(defun test-class (&rest args)
  "Shorthand for (make-instance 'test-class ...)"
  (apply #'make-instance 'test-class args))

(test stringify-class-includes-only-bound-slots
  (is-every equalp
    ((ph) (recode (test-class)))
    ((ph "a" 'null) (recode (test-class :a nil)))
    ((ph "b" #(1 2 3)) (recode (test-class :b '(1 2 3))))
    ((ph "a" 42 "b" #(1 2 3)) (recode (test-class :a 42 :b '(1 2 3))))))

(test stringify-class-uses-type-for-nil
  (is (equalp (ph "a" 'null "b" #() "c" nil) (recode (test-class :a nil :b nil :c nil)))))

(test stringify-class-recurses
  (is (equalp (ph "a" (ph "a" 42)) (recode (test-class :a (test-class :a 42))))))

(test stringify-class-in-plain-data
  (is (equalp (ph "a" (ph "a" 42)) (recode (ph "a" (test-class :a 42))))))

(defclass test-class-case ()
  ((all-upper :initform 0)
   (|mixedCase| :initform 0)))

(test stringify-class-downcases-symbols-except-mixed-case
  (is (equalp (ph "all-upper" 0 "mixedCase" 0) (recode (make-instance 'test-class-case)))))

(defclass test-class/stringify-coerced-fields ()
  ())

(defmethod jzon:coerced-fields ((a test-class/stringify-coerced-fields))
  (declare (ignore a))
  (list (list "foo" 42)
        (list "bar" 101.1d0)
        (list "baz" #(192 168 1 1))))

(test stringify-pretty-argorder-bugfix
  (is (string= "[
  {
    \"x\": 0
  }
]" (jzon:stringify (vector (ph "x" 0)) :pretty t))))

(test stringify-no-slots-on-unknown-object ()
  (is (string= "{}" (jzon:stringify (make-array '(2 2))))))

(test stringify-pretty-prints-keys
  (is (string= "{\"#(1 2)\":0}" (jzon:stringify (ph #(1 2) 0)))))

(test stringify-errors-on-circular-references
  (signals (jzon:json-recursive-write-error)
    (let ((ht (ph "x" 0)))
      (setf (gethash "self" ht) ht)
      (jzon:stringify ht))))

(test stringify-errors-on-circular-reference-during-pretty-print
  (signals (jzon:json-recursive-write-error)
    (let ((ht (ph "x" 0)))
      (setf (gethash "self" ht) ht)
      (jzon:stringify ht :pretty t))))

(test stringify-errors-on-circular-reference-vector-during-pretty-print
  (signals (jzon:json-recursive-write-error)
    (let ((v (vector 0 1 2)))
      (setf (aref v 0) v)
      (jzon:stringify v :pretty t))))

(test stringify-errors-on-non-symbol-coerce-key
  (signals (type-error) (jzon:stringify 42 :coerce-key 0)))

(test stringify-allows-symbols-on-coerce-key
  (finishes (jzon:stringify 42 :coerce-key (constantly "42"))))

(test stringify-coerce-key-calls-fn
  (is (string= "{\"something-else\":42}" (jzon:stringify (ph "something" 42) :coerce-key (constantly "something-else")))))

(test stringify-detects-alist-when-characters
  (is (string= "{\"a\":5,\"b\":42}" (jzon:stringify '((#\a . 5) (#\b . 42))))))

(test stringify-detects-alist-when-strings
  (is (string= "{\"a\":5,\"b\":42}" (jzon:stringify '(("a" . 5) ("b" . 42))))))

(test stringify-detects-alist-when-symbols
  (is (string= "{\"a\":5,\"b\":42}" (jzon:stringify '((#:a . 5) (#:b . 42))))))

(test stringify-no-alist-when-integers
  (is (string= "[[1,5],[2,42]]" (jzon:stringify '((1 5) (2 42))))))

(test stringify-detects-plist-when-characters
  (is (string= "{\"a\":5,\"b\":42}" (jzon:stringify '(#\a 5 #\b 42)))))

(test stringify-detects-plist-when-strings
  (is (string= "{\"a\":5,\"b\":42}" (jzon:stringify '("a" 5 "b" 42)))))

(test stringify-detects-plist-when-symbols
  (is (string= "{\"a\":5,\"b\":42}" (jzon:stringify '(#:a 5 #:b 42)))))

(test stringify-no-plist-when-integers
  (is (string= "[1,5,2,42]" (jzon:stringify '(1 5 2 42)))))

(test stringify-coerces-pathname-to-namestring
  (is (string= "\"hello.lisp\""(jzon:stringify #p"hello.lisp"))))

(test stringify-writes-cons-as-2-tuple
  (is (string= "[1,2]" (jzon:stringify (cons 1 2)))))

(test stringify-replacer-keeps-keys-on-t
  (is (string= "{\"x\":0}"
               (jzon:stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore k v))
                                      t)))))

(test stringify-replacer-filters-keys-on-nil
  (is (string= "{}"
               (jzon:stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t nil)))))))

(test stringify-replacer-filters-some-keys-on-nil
  (is (string= "{\"y\":0}"
               (jzon:stringify (ph :x 0 :y 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t (eq k :y))))))))

(test stringify-replacer-replaces-values-using-multiple-values
  (is (string= "{\"x\":42}"
               (jzon:stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t (values t 42))))))))

(test stringify-replacer-ignores-second-value-on-nil
  (is (string= "{}"
               (jzon:stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t (values nil 42))))))))

(test stringify-replacer-is-called-on-sub-objects
  (is (string= "{\"x\":{\"a\":42}}"
               (jzon:stringify (ph :x (ph :a 0))
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (if (eq k :a)
                                          (values t 42)
                                          t))))))

(test stringify-replacer-is-called-recursively
  (is (string= "{\"x\":{\"y\":{\"z\":0}}}"
               (jzon:stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        (:x (values t (ph :y 0)))
                                        (:y (values t(ph :z 0)))
                                        (t t)))))))

(test stringify-replacer-is-called-on-toplevel-value-with-nil-key
  (5am:is-true
   (let ((key-is-nil nil))
     (jzon:stringify 0 :replacer (lambda (k v)
                              (declare (ignore v))
                              (setf key-is-nil (null k))))
     key-is-nil))
  (5am:is-true
   (let ((value 0)
         (value-is-same nil))
     (jzon:stringify value :replacer (lambda (k v)
                                  (declare (ignore k))
                                  (setf value-is-same (eq value v) )))
     value-is-same)))

(test stringify-replacer-can-replace-toplevel-value
  (is (string= "42" (jzon:stringify 0 :replacer (lambda (k v)
                                             (declare (ignore k v))
                                             (values t 42))))))

(test stringify-replacer-is-called-on-array-elements-with-element-indexes
  (is (equalp #(0 1 2)
              (let ((keys (list)))
                (jzon:stringify #(t t t) :replacer (lambda (k v)
                                                (declare (ignore v))
                                                (when k
                                                  (push k keys))
                                                t)) ;; Gotta return t or it'll remove the elements
                (coerce (nreverse keys) 'vector)))))

(test stringify-replacer-is-only-called-with-nil-on-toplevel-value
  (is (equalp '(#(1 2 3))
              (let ((called-on (list)))
                (jzon:stringify #(1 2 3) :replacer (lambda (k v)
                                                (when (null k)
                                                  (push v called-on))
                                                t))
                (nreverse called-on)))))

(test stringify-replacer-is-called-on-plists-as-objects
  (is (string= "{\"a\":\"b\",\"b\":\"a\"}"
               ;; As a p-list
               (jzon:stringify '("a" 19 "b" "c")
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (cond
                                        ((equal k "a") (values t "b"))
                                        ((equal k "b") (values t "a"))
                                        (t t)))))))

(test stringify-replacer-is-called-on-alists-as-objects
  (is (string= "{\"a\":\"b\",\"b\":\"a\"}"
               ;; as an a-list
               (jzon:stringify '(("a" . 19) ("b" . "c"))
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (cond
                                        ((equal k "a") (values t "b"))
                                        ((equal k "b") (values t "a"))
                                        (t t)))))))

(def-suite jzon.json-checker :in jzon)

(in-suite jzon.json-checker)

;; fail1 in json-checker goes against RFC
;; (test fail1
;;   (signals jzon:json-parse-error (jzon:parse "\"A JSON payload should be an object or array, not a string.\"")))

(test fail2
  (signals jzon:json-parse-error (jzon:parse "[\"Unclosed array\"")))

(test fail3
  (signals jzon:json-parse-error (jzon:parse "{unquoted_key: \"keys must be quoted\"}")))

(test fail4
  (signals jzon:json-parse-error (jzon:parse "[\"extra comma\",]")))

(test fail5
  (signals jzon:json-parse-error (jzon:parse "[\"double extra comma\",,]")))

(test fail6
  (signals jzon:json-parse-error (jzon:parse "[   , \"<-- missing value\"]")))

(test fail7
  (signals jzon:json-parse-error (jzon:parse "[\"Comma after the close\"],")))

(test fail8
  (signals jzon:json-parse-error (jzon:parse "[\"Extra close\"]]")))

(test fail9
  (signals jzon:json-parse-error (jzon:parse "{\"Extra comma\": true,}")))

(test fail10
  (signals jzon:json-parse-error (jzon:parse "{\"Extra value after close\": true} \"misplaced quoted value\"")))

(test fail11
  (signals jzon:json-parse-error (jzon:parse "{\"Illegal expression\": 1 + 2}")))

(test fail12
  (signals jzon:json-parse-error (jzon:parse "{\"Illegal invocation\": alert()}")))

(test fail13
  (signals jzon:json-parse-error (jzon:parse "{\"Numbers cannot have leading zeroes\": 013}")))

(test fail14
  (signals jzon:json-parse-error (jzon:parse "{\"Numbers cannot be hex\": 0x14}")))

(test fail15
  (signals jzon:json-parse-error (jzon:parse "[\"Illegal backslash escape: \\x15\"]")))

(test fail16
  (signals jzon:json-parse-error (jzon:parse "[\\naked]")))

(test fail17
  (signals jzon:json-parse-error (jzon:parse "[\"Illegal backslash escape: \\017\"]")))

(test fail18
  (signals jzon:json-parse-error (jzon:parse "[[[[[[[[[[[[[[[[[[[[\"Too deep\"]]]]]]]]]]]]]]]]]]]]" :max-depth 19)))

(test fail19
  (signals jzon:json-parse-error (jzon:parse "{\"Missing colon\" null}")))

(test fail20
  (signals jzon:json-parse-error (jzon:parse "{\"Double colon\":: null}")))

(test fail21
  (signals jzon:json-parse-error (jzon:parse "{\"Comma instead of colon\", null}")))

(test fail22
  (signals jzon:json-parse-error (jzon:parse "[\"Colon instead of comma\": false]")))

(test fail23
  (signals jzon:json-parse-error (jzon:parse "[\"Bad value\", truth]")))

(test fail24
  (signals jzon:json-parse-error (jzon:parse "['single quote']")))

(test fail25
  (signals jzon:json-parse-error (jzon:parse "[\"	tab	character	in	string	\"]")))

(test fail26
  (signals jzon:json-parse-error (jzon:parse "[\"tab\\   character\\   in\\  string\\  \"]")))

(test fail27
  (signals jzon:json-parse-error (jzon:parse "[\"line
break\"]")))

(test fail28
  (signals jzon:json-parse-error (jzon:parse "[\"line\\
break\"]")))

(test fail29
  (signals jzon:json-parse-error (jzon:parse "[0e]")))

(test fail30
  (signals jzon:json-parse-error (jzon:parse "[0e+]")))

(test fail31
  (signals jzon:json-parse-error (jzon:parse "[0e+-1]")))

(test fail32
  (signals jzon:json-parse-error (jzon:parse "{\"Comma instead if closing brace\": true,")))

(test fail33
  (signals jzon:json-parse-error (jzon:parse "[\"mismatch\"}")))

(test pass1
  (is (equalp
       (vector "JSON Test Pattern pass1"
               (ph "object with 1 member" (vector "array with 1 element"))
               (ph)
               (vector)
               -42
               t
               nil
               'null
               (ph "integer" 1234567890
                   "real" -9876.54321d0
                   "e" 1.23456789d-13
                   "E" 1.23456789d34
                   "" 2.3456789011999997d76
                   "zero" 0
                   "one" 1
                   "space" " "
                   "quote" "\""
                   "backslash" "\\"
                   "controls" "
	"
                   "slash" "/ & /"
                   "alpha" "abcdefghijklmnopqrstuvwyz"
                   "ALPHA" "ABCDEFGHIJKLMNOPQRSTUVWYZ"
                   "digit" "0123456789"
                   "0123456789" "digit"
                   "special" "`1~!@#$%^&*()_+-={':[,]}|;.</>?"
                   "hex" "ģ䕧覫췯ꯍ"
                   "true" t
                   "false" nil
                   "null" 'null
                   "array" (vector)
                   "object" (ph)
                   "address" "50 St. James Street"
                   "url" "http://www.JSON.org/"
                   "comment" "// /* <!-- --"
                   "# -- --> */" " "
                   " s p a c e d " #(1 2 3 4 5 6 7)
                   "compact" #(1 2 3 4 5 6 7)
                   "jsontext" "{\"object with 1 member\":[\"array with 1 element\"]}"
                   "quotes" "&#34; \" %22 0x22 034 &#x22;"
                   "/\\\"쫾몾ꮘﳞ볚
	`1~!@#$%^&*()_+-=[]{}|;:',./<>?" "A key can be any string")
         0.5d0 98.6d0 99.44d0 1066 10.0d0 1.0d0 0.1d0 1.0d0 2.0d0 2.0d0 "rosebud")
       (jzon:parse "[
    \"JSON Test Pattern pass1\",
    {\"object with 1 member\":[\"array with 1 element\"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        \"integer\": 1234567890,
        \"real\": -9876.543210,
        \"e\": 0.123456789e-12,
        \"E\": 1.234567890E+34,
        \"\":  23456789012E66,
        \"zero\": 0,
        \"one\": 1,
        \"space\": \" \",
        \"quote\": \"\\\"\",
        \"backslash\": \"\\\\\",
        \"controls\": \"\\b\\f\\n\\r\\t\",
        \"slash\": \"/ & \\/\",
        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",
        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",
        \"digit\": \"0123456789\",
        \"0123456789\": \"digit\",
        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",
        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",
        \"true\": true,
        \"false\": false,
        \"null\": null,
        \"array\":[  ],
        \"object\":{  },
        \"address\": \"50 St. James Street\",
        \"url\": \"http://www.JSON.org/\",
        \"comment\": \"// /* <!-- --\",
        \"# -- --> */\": \" \",
        \" s p a c e d \" :[1,2 , 3

,

4 , 5        ,          6           ,7        ],\"compact\":[1,2,3,4,5,6,7],
        \"jsontext\": \"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",
        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",
        \"\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"
: \"A key can be any string\"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,\"rosebud\"]"))))

(test pass2
  (is (equalp #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#("Not too deep")))))))))))))))))))
              (jzon:parse "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))))

(test pass3
  (is (equalp (ph "JSON Test Pattern pass3"
                  (ph "The outermost value" "must be an object or array."
                      "In this test" "It is an object."))
              (jzon:parse "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\"
    }
}
"))))
