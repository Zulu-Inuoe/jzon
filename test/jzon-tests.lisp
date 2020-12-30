(defpackage #:com.inuoe.jzon-tests
  (:use #:cl)
  (:import-from
   #:alexandria
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
