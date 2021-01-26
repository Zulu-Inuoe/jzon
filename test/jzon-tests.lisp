(defpackage #:com.inuoe.jzon-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:hash-table-keys
   #:plist-hash-table)
  (:import-from
   #:com.inuoe.jzon
   #:coerced-fields
   #:coerce-key
   #:coerce-element
   #:json-parse-error
   #:parse
   #:stringify)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:is-every
   #:signals
   #:test)
  (:import-from #:uiop)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
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

(defun ph (&rest plist)
  "Shorthand for plist-hash-table."
  (plist-hash-table plist :test 'equal))

(test parses-atoms
  (is (eq 'null (parse "null")))
  (is (eq 't (parse "true")))
  (is (eq 'nil (parse "false"))))

(test parses-integers
  (is (integerp (parse "42")))
  (is (= 42 (parse "42"))))

(test parses-decimals
  (is (typep (parse "42.0") 'double-float))
  (is (= 42.0d0 (parse "42.0"))))

(test parses-exponent
  (is (typep (parse "42e1") 'double-float))
  (is (= 420.0d0 (parse "42e1"))))

(test parses-decimals-with-exponent
  (is (= 42.0d0 (parse "0.42e2"))))

(test parse-negative-decimal
  (is (= -0.1d0 (parse "-0.1"))))

(test disallows-leading-zeros
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "01"))
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "01.0"))
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "01e10")))

(test disallows-trailing-decimal-point
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "1."))
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "1.e10")))

(test disallows-trailing-exponent-marker
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "1e"))
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "1.0e"))
  (signals (com.inuoe.jzon:json-parse-error)
    (parse "0e")))

(test parses-arrays
  (is (equalp #() (parse "[]")))
  (is (equalp #(1 2 3) (parse "[1, 2, 3]"))))

(test parses-objects
  (is (equalp (ph) (parse "{}")))
  (is (equalp (ph "x" 1 "y" 2) (parse "{\"x\": 1, \"y\": 2}"))))

(test parse-singular
  (is (equalp (ph "foo" "bar")
              (parse "{\"foo\":\"bar\"}"))
      "Matching of a single simple string")
  (is (equalp (ph "bar" 1000)
              (parse "{\"bar\":1000}"))
      "Matching of a single number")
  (is (equalp (ph "bar" 10.1d0)
              (parse "{\"bar\":10.1}"))
      "Matching of a single decimal number")
  (is (equalp (ph "bar" #("foo" 10 101.1d0))
              (parse "{\"bar\":[\"foo\",10,101.10]}"))
      "Matching of an array with various types of elements"))

(test parse-multiple
  (is (equalp (ph "foo" "bar" "baz" "bang" "bing" 100 "bingo" 1.1d0 "bazo" #(1 2 "foo"))
              (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":100,\"bingo\":1.1,\"bazo\":[1,2,\"foo\"]}"))
      "Parsing of multiple items of all kinds"))

(test parse-nested
  (is (equalp (ph "foo" (ph "bar" "baz"))
              (parse "{\"foo\":{\"bar\":\"baz\"}}"))
      "One object in one object")
  (is (equalp (ph "foo" "bar" "bie" (ph "bar" "baz" "bang" 1000) "bing" "bingo")
              (parse "{\"foo\":\"bar\",\"bie\":{\"bar\":\"baz\",\"bang\":1000},\"bing\":\"bingo\"}")))
  (is (equalp (ph "foo" (vector (ph "foo" "bar" "baz" 1000)))
              (parse "{\"foo\":[{\"foo\":\"bar\",\"baz\":1000}]}"))
      "Object in an array")
  (is (equalp (ph  "foo" "bar" "baz" (ph "boo" 100.1d0))
              (parse "{\"foo\":\"bar\",\"baz\":{\"boo\":100.10}}"))
      "Decimal number in inner object"))

(test unicode-chars
  (is (equalp (ph "λlambda" "💩poop")
              (parse "{\"\\u03BBlambda\":\"\\ud83d\\udca9poop\"}")))

  (is (equalp (ph "lambdaλ" "poop💩")
              (parse "{\"lambda\\u03BB\":\"poop\\ud83d\\udca9\"}")))

  (is (equalp (ph "lambdaλlambda" "poop💩poop")
              (parse "{\"lambda\\u03BBlambda\":\"poop\\ud83d\\udca9poop\"}"))))

(test parse-pools-keys
  (let* ((objects (parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]"))
         (keys (map 'list #'hash-table-keys objects))
         (xs (mapcar #'first keys))
         (x1 (first xs))
         (x2 (second xs))
         (x3 (third xs)))
    (is (eq x1 x2))
    (is (eq x2 x3))))

(test parse-uses-custom-key-fn
  (let ((keys ()))
    (parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn (lambda (key) (push key keys) key))
    (is (equalp '("x" "x" "x") keys))))

(test parse-ignores-pre-post-whitespace
  (is-every equalp
    (nil               (parse "  false "))
    (t                 (parse " true  "))
    ('null             (parse "   null "))
    (42                (parse "      42 "))
    (42.0d0            (parse "  42e0  "))
    ("Hello, world!"   (parse "   \"Hello, world!\"  "))
    (#(1 2 3)          (parse " [1,2,3]  "))
    ((ph "x" 10 "y" 0) (parse "   { \"x\": 10, \"y\": 0}   "))))

(test parse-accepts-stream
  (flet ((parse (str)
           (with-input-from-string (in str)
             (parse in))))
    (is-every equalp
      (nil               (parse "false"))
      (t                 (parse "true"))
      ('null             (parse "null"))
      (42                (parse "42"))
      (42.0d0            (parse "42e0"))
      ("Hello, world!"   (parse "\"Hello, world!\""))
      (#(1 2 3)          (parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-accepts-pathname
  (flet ((parse (str)
           (uiop:with-temporary-file (:stream stream :pathname p :external-format :utf-8)
             (write-string str stream)
             (finish-output stream)
             (close stream)
             (parse p))))
    (is-every equalp
      (nil               (parse "false"))
      (t                 (parse "true"))
      ('null             (parse "null"))
      (42                (parse "42"))
      (42.0d0            (parse "42e0"))
      ("Hello, world!"   (parse "\"Hello, world!\""))
      (#(1 2 3)          (parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-accepts-non-simple-string
  (flet ((parse (str)
           (parse (make-array (length str) :element-type 'character :fill-pointer t :initial-contents str))))
    (is-every equalp
      (nil               (parse "false"))
      (t                 (parse "true"))
      ('null             (parse "null"))
      (42                (parse "42"))
      (42.0d0            (parse "42e0"))
      ("Hello, world!"   (parse "\"Hello, world!\""))
      (#(1 2 3)          (parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (parse "{ \"x\": 10, \"y\": 0}")))))

(test stringify-to-nil-returns-string
  (is (string= "42" (stringify 42))))

(test stringify-to-t-writes-to-stdout
  (is (string= "42" (with-output-to-string (*standard-output*)
                      (stringify 42 :stream t)))))

(test stringify-to-stream-writes-to-stream
  (is (string= "42" (with-output-to-string (stream)
                      (stringify 42 :stream stream)))))

(test stringify-pretty-array-spaces-elements
  (is (string= "[ 1, 2, 3 ]" (stringify #(1 2 3) :pretty t))))

(test stringify-pretty-object-spaces-kv
  (is (string= "{ \"x\": 0 }" (stringify (ph "x" 0) :pretty t))))

(test stringify-pretty-object-newlines-multiple-kv
  (is (string= "{
  \"x\": 0,
  \"y\": 5
}" (stringify (ph "x" 0 "y" 5) :pretty t))))

(test stringify-pretty-object-newlines-if-nested-object
  (is (string= "{
  \"obj\": {
    \"x\": 0,
    \"y\": 5
  }
}" (stringify (ph "obj" (ph "x" 0 "y" 5)) :pretty t))))

(test stringify-pretty-array-newlines-if-nested-object
  (is (string= "[
  1,
  {
    \"x\": 0,
    \"y\": 5
  }
]" (stringify (vector 1 (ph "x" 0 "y" 5)) :pretty t))))

(test string-expands-special-escapes
  (is-every string=
    (#\Backspace (parse "\"\\b\""))
    (#\Formfeed  (parse "\"\\f\""))
    (#\Linefeed  (parse "\"\\n\""))
    (#\Return    (parse "\"\\r\""))
    (#\Tab       (parse "\"\\t\""))))

(test stringify-atoms
  (is-every string=
    ("true" (stringify t))
    ("false" (stringify nil))
    ("null" (stringify 'null))))

(test stringify-integers
  (is (string= "5" (stringify 5)))
  (is (string= "0" (stringify 0))))

(test stringify-strings
  (is (string= "\"hello, world!\"" (stringify "hello, world!")))
  (is (string= "\"\"" (stringify ""))))

(test stringify-string-handles-special-escapes
  (is-every string=
    ("\"\\b\"" (stringify (string #\Backspace)))
    ("\"\\f\"" (stringify (string #\Formfeed)))
    ("\"\\n\"" (stringify (string #\Linefeed)))
    ("\"\\r\"" (stringify (string #\Return)))
    ("\"\\t\"" (stringify (string #\Tab)))))

(test stringify-array
  (is (string= "[]" (stringify #())))
  (is (string= "[42,\"hello\",[]]" (stringify #(42 "hello" #())))))

(defun recode (value)
  "Shorthand for (parse (stringify value))"
  (parse (stringify value)))

(test stringify-object
  (is (string= "{}" (stringify (ph))))
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

(test coerce-element-uses-coerced-fields-spec
  (let ((coerced (coerce-element (make-instance 'test-class/stringify-coerced-fields) #'coerce-key)))
    (is-every equalp
      (42 (gethash "foo" coerced))
      (101.1d0 (gethash "bar" coerced))
      (#(192 168 1 1) (gethash "baz" coerced)))))

(test stringify-pretty-argorder-bugfix
  (is (string= "[ { \"x\": 0 } ]" (stringify (vector (ph "x" 0)) :pretty t))))

(def-suite jzon.json-checker :in jzon)

(in-suite jzon.json-checker)

;; fail1 in json-checker goes against RFC
;; (test fail1
;;   (signals jzon:json-parse-error (parse "\"A JSON payload should be an object or array, not a string.\"")))

(test fail2
  (signals jzon:json-parse-error (parse "[\"Unclosed array\"")))

(test fail3
  (signals jzon:json-parse-error (parse "{unquoted_key: \"keys must be quoted\"}")))

(test fail4
  (signals jzon:json-parse-error (parse "[\"extra comma\",]")))

(test fail5
  (signals jzon:json-parse-error (parse "[\"double extra comma\",,]")))

(test fail6
  (signals jzon:json-parse-error (parse "[   , \"<-- missing value\"]")))

(test fail7
  (signals jzon:json-parse-error (parse "[\"Comma after the close\"],")))

(test fail8
  (signals jzon:json-parse-error (parse "[\"Extra close\"]]")))

(test fail9
  (signals jzon:json-parse-error (parse "{\"Extra comma\": true,}")))

(test fail10
  (signals jzon:json-parse-error (parse "{\"Extra value after close\": true} \"misplaced quoted value\"")))

(test fail11
  (signals jzon:json-parse-error (parse "{\"Illegal expression\": 1 + 2}")))

(test fail12
  (signals jzon:json-parse-error (parse "{\"Illegal invocation\": alert()}")))

(test fail13
  (signals jzon:json-parse-error (parse "{\"Numbers cannot have leading zeroes\": 013}")))

(test fail14
  (signals jzon:json-parse-error (parse "{\"Numbers cannot be hex\": 0x14}")))

(test fail15
  (signals jzon:json-parse-error (parse "[\"Illegal backslash escape: \\x15\"]")))

(test fail16
  (signals jzon:json-parse-error (parse "[\\naked]")))

(test fail17
  (signals jzon:json-parse-error (parse "[\"Illegal backslash escape: \\017\"]")))

(test fail18
  (signals jzon:json-parse-error (parse "[[[[[[[[[[[[[[[[[[[[\"Too deep\"]]]]]]]]]]]]]]]]]]]]" :maximum-depth 20)))

(test fail19
  (signals jzon:json-parse-error (parse "{\"Missing colon\" null}")))

(test fail20
  (signals jzon:json-parse-error (parse "{\"Double colon\":: null}")))

(test fail21
  (signals jzon:json-parse-error (parse "{\"Comma instead of colon\", null}")))

(test fail22
  (signals jzon:json-parse-error (parse "[\"Colon instead of comma\": false]")))

(test fail23
  (signals jzon:json-parse-error (parse "[\"Bad value\", truth]")))

(test fail24
  (signals jzon:json-parse-error (parse "['single quote']")))

(test fail25
  (signals jzon:json-parse-error (parse "[\"	tab	character	in	string	\"]")))

(test fail26
  (signals jzon:json-parse-error (parse "[\"tab\\   character\\   in\\  string\\  \"]")))

(test fail27
  (signals jzon:json-parse-error (parse "[\"line
break\"]")))

(test fail28
  (signals jzon:json-parse-error (parse "[\"line\\
break\"]")))

(test fail29
  (signals jzon:json-parse-error (parse "[0e]")))

(test fail30
  (signals jzon:json-parse-error (parse "[0e+]")))

(test fail31
  (signals jzon:json-parse-error (parse "[0e+-1]")))

(test fail32
  (signals jzon:json-parse-error (parse "{\"Comma instead if closing brace\": true,")))

(test fail33
  (signals jzon:json-parse-error (parse "[\"mismatch\"}")))

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
       (parse "[
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
              (parse "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))))

(test pass3
  (is (equalp (ph "JSON Test Pattern pass3"
                  (ph "The outermost value" "must be an object or array."
                      "In this test" "It is an object.")) 
              (parse "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\"
    }
}
"))))
