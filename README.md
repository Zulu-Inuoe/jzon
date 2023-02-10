# jzon

A correct and safe(er) JSON [RFC 8259][JSONRFC] parser with batteries included.

[![Actions Status](https://github.com/Zulu-Inuoe/jzon/workflows/ci/badge.svg)](https://github.com/Zulu-Inuoe/jzon/actions)

#### Table of Contents

* [Type Mappings](#type-mappings)
* [Reading](#reading)
  * [Incremental Parser](#incremental-parser)
    * [Example](#incremental-parser-example)
* [Writing](#writing)
  * [Symbol key case](#symbol-key-case)
  * [Incremental Writer](#incremental-writer)
    * [Example](#incremental-writer-example)
* [Custom Serialization](#custom-serialization)
  * [standard-object](#standard-object)
  * [coerced-fields](#coerced-fields)
  * [write-value](#write-value)
* [Features](#features)
  * [Safety](#safety)
  * [Correctness](#correctness)
  * [Simplicity](#simplicity)
  * [Performance](#performance)
* [Dependencies](#dependencies)
* [License](#license)
* [Alternatives](#alternatives)

# Type Mappings

`jzon` cannonically maps types per the following chart:

| JSON   | CL                      |
|--------|-------------------------|
| true   | symbol `t`              |
| false  | symbol `nil`            |
| null   | symbol `null`           |
| number | integer or double-float |
| string | simple-string           |
| array  | simple-vector           |
| object | hash-table (equal)      |

**Note** the usage of symbol `cl:null` as a sentinel for JSON `null`

These are the values returned by the [reading](#reading) functions, though when [writing](#writing), other values are supported.

# Reading

`jzon:parse` will parse JSON and produce a CL value:

``` common-lisp
(jzon:parse "
{
  \"name\": \"Rock\",
  \"coords\": {
    \"x\": 5,
    \"y\": 7
  },
  \"attributes\": [\"fast\", \"hot\"],
  \"physics\": true,
  \"item\": false,
  \"parent\": null
}")
; => #<HASH-TABLE :TEST EQUAL :COUNT 6 {1003ECDC93}>

(defparameter *ht* *)

(string= (gethash *ht* "name") "Rock")                  ; => t
(= (gethash (gethash *ht* "coords") "x") 5)             ; => t
(= (gethash (gethash *ht* "coords") "y") 7)             ; => t
(equalp (gethash *ht* "attributes") #("fast" "hot"))    ; => t
(eq (gethash *ht* "physics") "t")                       ; => t
(eq (gethash *ht* "item") nil)                          ; => t
(eq (gethash *ht* "parent") 'null)                      ; => t
```

`jzon:parse in` reads input from `in` and returns a parsed value per [Type Mappings](#type-mappings).

`in` can be any of the following:
* string
* (vector (unsigned-byte 8)) - octets in utf-8
* stream - character or binary in utf-8
* pathname - `jzon:parse` will open the file for reading in utf-8

`jzon:parse` also accepts the follwing keyword arguments:
* `:allow-comments` - *false* This allows the given JSON to contain cpp-style `// line comments` and `/* block comments */`.
* `:allow-trailing-comma` - *false* This allows for a single trailing comma after the final element in a JSON array or object.
* `:max-depth` This controls the maximum depth to allow arrays/objects to nest. Can be a positive integer, or `nil` to disable depth tests.
* `:max-string-length` This controls the maximum length of strings. This applies for both keys and values. Must be a positive integer no larger than `array-dimension-limit`.
* `:key-fn` A function of one argument responsible for 'interning' object keys. Should accept a `simple-string` and return the 'interned' key

**Tip**: `key-fn` can be supplied as `#'identity` in order to disable [key pooling](#object-key-pooling):

``` common-lisp
(jzon:parse "[ { \"x\": 1, \"y\": 2 }, { \"x\": 3, \"y\": 4 } ]" :key-fn #'identity)

(defparameter *v* *)

(gethash "x" (aref *v* 0)) ; => 1
(gethash "y" (aref *v* 0)) ; => 2
```

This may speed up parsing on certain JSON as we do not have to build the string lookup table.

**Tip**: `alexandria:make-keyword` or equivalent can be used to make object keys into symbols:

``` common-lisp
(jzon:parse "[ { \"x\": 1, \"y\": 2 }, { \"x\": 3, \"y\": 4 } ]" :key-fn #'alexandria:make-keyword)

(defparameter *v* *)

(gethash :|x| (aref *v* 0)) ; => 1
(gethash :|y| (aref *v* 0)) ; => 2
```

.. or if you want to follow as default CL does, you can `string-upcase` before `intern`ing:

```lisp
(jzon:parse "[ { \"x\": 1, \"y\": 2 }, { \"x\": 3, \"y\": 4 } ]" :key-fn (lambda (k) (alexandria:make-keyword (string-upcase k))))

(defparameter *v* *)

(gethash :x (aref *v* 0)) ; => 1
(gethash :y (aref *v* 0)) ; => 2
```

## Incremental Parser

In addition to `jzon:parse`, `jzon:with-parser` exposes an incremental parser for reading JSON in parts:

```lisp
(jzon:with-parser (parser "{\"x\": 1, \"y\": [2, 3], \"live\": false}")
  (jzon:parse-next parser)  ; => :begin-object
  (jzon:parse-next parser)  ; => :object-key, "x"
  (jzon:parse-next parser)  ; => :value, 1
  (jzon:parse-next parser)  ; => :object-key, "y"
  (jzon:parse-next parser)  ; => :begin-array
  (jzon:parse-next parser)  ; => :value, 2
  (jzon:parse-next parser)  ; => :value, 3
  (jzon:parse-next parser)  ; => :end-array
  (jzon:parse-next parser)  ; => :object-key, "live"
  (jzon:parse-next parser)  ; => :value, nil
  (jzon:parse-next parser)  ; => :end-object
  (jzon:parse-next parser)) ; => nil
```

both `jzon:with-parser` and `jzon:make-parser` receive the same arguments as `jzon:parse`.

**Note**: `jzon:make-parser` is akin to `cl:open` and `jzon:close-parser` is akin to `cl:close`. Prefer `jzon:with-parser` when you do not need indefinite extent for the parser.

The relevant functions for the incremental parser are:

`jzon:make-parser in` - Construct a parser from `in`, which may be any of the inputs applicable to `jzon:parse`

`jzon:parse-next parser` - Parse the next token from `parser`. Returns two values, depending on the token:
  * `:value`, `<value>` - The parser encountered a `json-atom`, `<value>` is the value of the atom
  * `:begin-object`, `nil` - The parser encountered an object opening
  * `:object-key`, `<key>` - The parser encountered an object key, `<key>` is that key
  * `:close-object`, `nil` - The parser encountered an object closing
  * `:begin-array`, `nil` - The parser encountered an array opening
  * `:close-array`, `nil` - The parser encountered an array closing
  * nil - The parser is complete

`jzon:close-parser parser` - Close a parser, closing any opened streams and allocated objects


### Incremental Parser Example

`jzon:parse` could be approximately defined as follows:

```lisp
(defun my/jzon-parse (in)
  (jzon:with-parser (parser in)
    (let (top stack key)
      (flet ((finish-value (value)
                (typecase stack
                  (null                 (setf top value))
                  ((cons list)          (push value (car stack)))
                  ((cons hash-table)    (setf (gethash (pop key) (car stack)) value)))))
        (loop
          (multiple-value-bind (evt value) (jzon:parse-next parser)
            (ecase evt
              ((nil)          (return top))
              (:value         (finish-value value))
              (:begin-array   (push (list) stack))
              (:end-array     (finish-value (coerce (the list (nreverse (pop stack))) 'simple-vector)))
              (:begin-object  (push (make-hash-table :test 'equal) stack))
              (:object-key    (push value key))
              (:end-object    (finish-value (pop stack))))))))))
```

# Writing

`jzon:stringify` will serialize an object to JSON:

``` common-lisp
(jzon:stringify #("Hello, world!" 5 2.2 #(null)))
; => "[\"Hello, world!\",5,2.2,[null]]"
```

`jzon:stringify` accepts the following keyword arguments:
* `:stream` A destination like in `format`, or a `pathname`. Like `format`, returns a string if `nil`.
* `:pretty` If true, output pretty-formatted JSON.
* `:coerce-element` A function for coercing 'non-native' values to JSON. See [Custom Serialization](#custom-serialization)
* `:coerce-key` A function for coercing key values to strings. See [Custom Serialization](#custom-serialization)

In addition to the mappings defined in [Type Mappings](#type-mappings), `jzon:stringify` accepts the following types of values:


| CL                | JSON                                                                |
|-------------------|---------------------------------------------------------------------|
| symbol            | string (`symbol-name`), but see [Symbol key case](#symbol-key-case) |
| real              | number                                                              |
| alist\*           | object                                                              |
| plist\*           | object                                                              |
| list              | array                                                               |
| sequence          | array                                                               |
| standard-object   | object                                                              |
| structure-object† | object                                                              |

\*: Heuristic depending on the key values - Detects alists/plists by testing each key to be a character, string, or symbol.

†: On supported implementations where structure slots are available via the MOP.

**Note**: These coercion rules only apply when using the default `:coerce-element` and `:coerce-key`.

## Symbol key case

When symbols are used as keys in objects, their names will be downcased, unless they contain mixed-case characters.

For example:

``` common-lisp
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash 'all-upper ht) 0)
  (setf (gethash '|mixedCase| ht) 0)
  (setf (gethash "ALL UPPER" ht) 0)

  (jzon:stringify ht :pretty t :stream t))
```

result:

``` json
{
  "all-upper": 0,
  "mixedCase": 0,
  "ALL UPPER": 0
}
```

This is particularly important when serializing CLOS objects per [Custom Serialization](#custom-serialization).

## Incremental Writer

In addition to `jzon:stringify`, `jzon:make-writer` exposes an incremental writer for writing JSON in parts.

``` common-lisp
(jzon:with-writer* (:stream *standard-output* :pretty t)
  (jzon:with-object*
    (jzon:write-properties* :age 24 "colour" "blue")
    (jzon:write-key* 42)
    (jzon:write-value* #(1 2 3))

    (jzon:write-key* "an-array")
    (jzon:with-array*
      (jzon:write-values* :these :are :array :elements))

    (jzon:write-key* "another array")
    (jzon:write-array* :or "you" "can" "use these" "helpers")))
; =>
```

```json
{
  "age": 24,
  "colour": "blue",
  "42": [
    1,
    2,
    3
  ],
  "an-array": [
    "THESE",
    "ARE",
    "ARRAY",
    "ELEMENTS"
  ],
  "another array": [
    "OR",
    "you",
    "can",
    "use these",
    "helpers"
  ]
}
```

`jzon:make-writer` and `jzon:with-writer*` accept the same arguments as `jzon:stringify`, *except* `:stream` must be an open `cl:stream`.

**Note** all writer functions have `*`-suffixed variants which use the `jzon:*writer*` variable and omit the first `writer` parameter.

eg instead of
```lisp
(let ((writer (jzon:make-writer)))
  (write-value writer "foo"))
```
we can use
```lisp
(jzon:with-writer* ()
  (write-value* "foo"))
```

### Incremental Writer Functions

The relevant functions for the incremental writer are:

`jzon:write-value writer value` - Writes any `value` to the `writer`. Usable when writing a toplevel value, array element, or object property value.

```lisp
(jzon:write-value* "Hello, world")
```

**Note**: This is a `generic-function` you can specialize your values on. See [custom serialization](#custom-serialization) for more information.

`jzon:with-array writer` - Open a block to begin writing array values.

```lisp
(jzon:with-array*
  (jzon:write-value* 0)
  (jzon:write-value* 1)
  (jzon:write-value* 2))
```

`jzon:begin-array writer` - Begin writing an array

```lisp
(jzon:begin-array*)
(jzon:write-value* 0)
(jzon:write-value* 1)
(jzon:write-value* 2)
(jzon:end-array*)
```

`jzon:write-values writer &rest values*` - Write several array values.
```lisp
(jzon:with-array*
  (jzon:write-values* 0 1 2))
```

`json:end-array writer` - Finish writing an array.

```lisp
(jzon:begin-array*)
(jzon:write-value* 0)
(jzon:write-value* 1)
(jzon:write-value* 2)
(jzon:end-array*)
```

`jzon:write-array` - Open a new array, write its values, and close it.

```lisp
(jzon:write-array* 0 1 2)
```

`jzon:with-object writer` - Open a block where you can begin writing object properties.

```lisp
(jzon:with-object*
  (jzon:write-property* "age" 42))
```

`jzon:begin-object writer` - Begin writing an object.

```lisp
(jzon:begin-object*)
(jzon:write-property* "age" 42)
(jzon:end-object*)
```

`jzon:write-key writer key` - Write an object key.

```lisp
(jzon:with-object*
  (jzon:write-key* "age")
  (jzon:write-value* 42))
```

`json:write-property writer key value` - Write an object key and value.

```lisp
(jzon:with-object*
  (jzon:write-property* "age" 42))
```

`jzon:write-properties writer &rest key* value*` - Write several object keys and values.

```lisp
(jzon:with-object*
  (jzon:write-properties* "age" 42
                          "colour" "blue"
                          "x" 0
                          "y" 10))
```

`json:end-object writer` - Finish writing an object.

```lisp
(jzon:begin-object*)
(jzon:write-property* "age" 42)
(jzon:end-object*)
```

`jzon:write-object writer &rest key* value*` - Open a new object, write its keys and values, and close it.

```lisp
(jzon:write-object* "age" 42
                    "colour" "blue"
                    "x" 0
                    "y" 10)
```


### Incremental Writer Example

`jzon:stringify` could be approximately defined as follows:

```lisp
(defun my/jzon-stringify (value)
  (labels ((recurse (value)
             (etypecase value
               (jzon:json-atom
                 (jzon:write-value* value))
               (vector
                 (jzon:with-array*
                   (map nil #'recurse value)))
               (hash-table
                 (jzon:with-object*
                   (maphash (lambda (k v)
                              (jzon:write-key* k)
                              (recurse v))
                            value))))))
    (with-output-to-string (s)
      (jzon:with-writer* (:stream s)
        (recurse value)))))
```

**Tip**: Every function returns the `jzon:writer` itself for usage with arrow macros:

``` common-lisp
(let ((writer (jzon:make-writer :stream *standard-output*)))
  (jzon:with-object writer
    (-> writer
        (jzon:write-key "key")
        (jzon:write-value "value")
        (jzon:begin-array)
        (jzon:write-value 1)
        (jzon:end-array))))`
```

# Custom Serialization

When using either `jzon:stringify` or `jzon:write-value`, you can customize writing of any values not covered in the [Type Mappings](#type-mappings) in an few different ways.

## standard-object

By default, if your object is a `standard-object`, it will be serialized as a JSON object, using each of its **bound** slots as keys.

Consider the following classes:

``` common-lisp
(defclass coordinate ()
  ((reference
    :initarg :reference)
   (x
    :initform 0
    :initarg :x
    :accessor x)
   (y
    :initform 0
    :initarg :y
    :accessor y)))

(defclass object ()
  ((alive
    :initform nil
    :initarg :alive
    :type boolean)
   (coordinate
    :initform nil
    :initarg :coordinate
    :type (or null coordinate))
   (children
    :initform nil
    :initarg :children
    :type list)))
```

If we stringify a fresh `coordinate` object via `(jzon:stringify (make-instance 'coordinate) :pretty t :stream t)`, we'd end up with:

``` json
{
  "x": 0,
  "y": 0
}
```

And if we `(jzon:stringify (make-instance 'coordinate :reference "Earth") :pretty t :stream t)`:

``` json
{
  "reference": "Earth",
  "x": 0,
  "y": 0
}
```

Similarly if we `(jzon:stringify (make-instance 'object) :pretty t :stream t)`:

``` json
{
  "alive": false,
  "coordinate": null,
  "children": []
}
```

Note that here we have `nil` representing `false`, `null`, and `[]`. This is done by examining the `:type` of each slot.
If no type is provided, `nil` shall serialize as `null`.

## coerced-fields

`jzon:coerced-fields` is a generic function which calculates the JSON object key/value pairs when writing and is a simple way to add custom serialization for your values.

Consider our previous `coordinate` class. If we always wanted to serialize only the `x` and `y` slots, and wanted to rename them, we could specialize `jzon:coerced-fields` as follows:

``` common-lisp
(defmethod jzon:coerced-fields ((coordinate coordinate))
  (list (list "coord-x" (x coordinate))
        (list "coord-y" (y coordinate))))
```

This results in:

``` json
{
  "coord-x": 0,
  "coord-y": 0
}
```

`jzon:coerced-fields` should return a list of 'fields', which are two (or three) element lists of the form:

``` common-lisp
(name value &optional type)
```

* `name` can be any suitable key name. In particular, integers are allowed coerced to their decimal string representation.
* `value` can be any value - it'll be coerced if necessary.
* `type` is used as `:type` above, in order to resolve ambiguities with `nil`.

### Example: Including only some slots

If the default `jzon:coerced-fields` gives you most of what you want, you can exclude/rename/add fields by specializing an `:around` method as follows:

``` common-lisp
(defmethod jzon:coerced-fields :around ((coordinate coordinate))
  (let* (;; Grab default fields
         (fields (call-next-method))
         ;; All fields except "children"
         (fields (remove 'children fields :key #'first))
         ;; Include a 'fake' field "name"
         (fields (cons (list 'name "Mary") fields)))
    fields))
```

This would result in the following:

``` json
{
  "name": "Mary",
  "alive": false,
  "coordinate": {
    "x": 0,
    "y": 0
  }
}
```

## write-value

For more fine-grained control, you can specialize a method on `jzon:write-value`.

This allows you to emit whatever value you wish for a given object.

`jzon:write-value writer value`

`writer` is a [writer](#incremental-writer) on which any of the writer functions may be called to serialize your object in any desired way.


``` common-lisp
(defclass my-point () ())

(defmethod jzon:write-value (writer (value my-point))
  (jzon:write-array writer 1 2))
```

See [writer](#incremental-writer) for the available functions.

# Features

In writing `jzon`, we prioritize the following properties, in order:

* [Safety](#safety)
* [Correctness](#correctness)
* [Simplicity](#simplicity)
* [Performance](#performance)

## Safety

[RFC 8259][JSONRFC] allows setting limits on things such as:
* Number values accepted
* Nesting level of arrays/objects
* Length of strings

We should be safe in the face of untrusted JSON and will error on 'unreasonable' input out-of-the-box, such as deeply nested objects or overly long strings.

### Type Safety

All of `jzon`'s public API's are type safe, issuing `cl:type-error` as appropriate.

Some other JSON parsers will make dangerous use of features like `optimize (safety 0) (speed 3)` in unreasonable places can cause errors such as:

``` lisp
CL-USER> (parse 2)
; Debugger entered on #<SB-SYS:MEMORY-FAULT-ERROR {1003964833}>
```

... which are unreasonable.

### Avoid Infinite Interning

`jzon` chooses to (by default) keep object keys as strings. Some libraries choose to `intern` object keys in some package. This is dangerous in the face of untrusted JSON, as every unique key read will be added to that package and never garbage collected.

### Avoid Stack Exhaustion

`jzon:parse` is written in an iterative way which avoids exhausting the call stack. In addition, we provide `:max-depth` to guard against unreasonable inputs.
For even more control, you can make use of the `jzon:with-parser` API's to avoid consing large amounts of user-supplied data to begin with.

## Correctness

This parser is written against [RFC 8259][JSONRFC] and strives to adhere strictly for maximum compliance and few surprises.

It also has been tested against the [JSONTestSuite][JSONTestSuite]. See the [JSONTestSuite](JSONTestSuite/) directory in this repo for making & running the tests.

In short, `jzon` is the only CL JSON library which correctly:
* *declines* all invalid inputs per that suite
* *accepts* all valid inputs per that suite

Additionally, `jzon` is one of a couple which never hard crash due to edge-cases like deeply nested objects/arrays.

### Unambiguous values

Values are never ambiguous between `[]`, `false`, `{}`, `null`, or a missing key.

### Compatible Float IO

While more work is doubtlessly necessary to validate further, care has been taken to ensure floating-point values are not lost between `(jzon:parse (jzon:stringify f))`, even across CL implementations.

In particular, certain edge-case values such as subnormals shall parse `===` with JavaScript parsing libraries.

## Simplicity

You call `jzon:parse`, and you get a reasonably standard CL object back.
You call `jzon:stringify` with a reasonably standard CL object and you should get reasonable JSON.

* No custom data structures or accessors required
* No worrying about key case auto conversion or hyphens/underscores being converted.
* No worrying about what package symbols are interned in (no symbols).
* No worrying about dynamic variables affecting a parse as in cl-json, jonathan, jsown. Everything affecting `jzon:parse` is given at the call-site.

`jzon:parse` also accepts either a string, octet vector, stream, or pathname for simpler usage over libraries requiring one or the other, or having separate parse functions.

Finally, all public API's strive to have good defaults so things 'Just Work'.

## Performance

While parsing, `jzon` at worst performs at 50% the speed of [jsown][jsown], while outperforming all other libraries.

And this is all while having the safety and correctness guarantees noted above.

### Object key pooling

`jzon` will use a key pool per-parse, causing shared keys in a nested JSON object to share keys:

``` common-lisp
(jzon:parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]")
```
In this example, the string `x` is shared (eq) between all 3 objects.

This optimizes for the common case of reading a JSON payload containing many duplicate keys.

### `base-string` coercion

When possible, strings will be coerced to `cl:simple-base-string`. This can lead to upwards of 1/4 memory usage per string on implementations like SBCL, which store `string`s internally as UTF32, while `base-string` can be represented in 8 bits per char.

# Dependencies

* [closer-mop](https://github.com/pcostanza/closer-mop)
* [flexi-streams](https://github.com/edicl/flexi-streams)
* [float-features](https://github.com/Shinmera/float-features)
* [uiop](https://gitlab.common-lisp.net/asdf/asdf)

# License

See [LICENSE](LICENSE).

`jzon` was originally a fork of [st-json](https://marijnhaverbeke.nl/st-json/), but I ended up scrapping all of the code except for for the function decoding Unicode.

# Alternatives

There are many CL JSON libraries available, and I defer to Sabra Crolleton's definitive list and comparisons [https://sabracrolleton.github.io/json-review](https://sabracrolleton.github.io/json-review).

But for posterity, included in this repository is a set of tests and results for the following libraries:

* [cl-json][cl-json]
* [jonathan][jonathan]
* [json-streams][json-streams]
* [jsown][jsown]
* [shasht][shasht]
* [yason][yason]

I believe `jzon` to be the superiour choice and hope for it to become the new, true de-facto library in the world of JSON-in-CL once and for all.

[JSONRFC]: https://tools.ietf.org/html/rfc8259
[JSONTestSuite]: https://github.com/nst/JSONTestSuite
[jsown]: https://github.com/madnificent/jsown
[cl-json]: https://cl-json.common-lisp.dev/cl-json.html
[jonathan]: https://github.com/Rudolph-Miller/jonathan
[json-streams]: https://github.com/rotatef/json-streams
[shasht]: https://github.com/yitzchak/shasht
[yason]: https://github.com/phmarek/yason
