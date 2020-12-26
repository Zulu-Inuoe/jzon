(defpackage #:com.inuoe.jzon-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:plist-hash-table)
  (:import-from
   #:com.inuoe.jzon
   #:parse
   #:json-parse-error)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:signals
   #:test)
  (:export
   #:jzon
   #:run
   #:main))

(in-package #:com.inuoe.jzon-tests)

(def-suite jzon
  :description "Tests for the seq library.")

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
