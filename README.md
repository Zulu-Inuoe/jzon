# jzon

A correct and safe(er) JSON [RFC 8259][JSONRFC] parser with batteries-included.

[![Actions Status](https://github.com/Zulu-Inuoe/jzon/workflows/ci/badge.svg)](https://github.com/Zulu-Inuoe/jzon/actions)

#### Table of Contents
* [Overview](#overview)
* [Usage](#usage)
  * [Type Mappings](#type-mappings)
  * [Reading](#reading)
  * [Writing](#writing)
    * [Symbol key case](#symbol-key-case)
    * [Custom Serialization](#custom-serialization)
      * [coerced-fields](#coerced-fields)
  * [Features](#features)
    * [Unambiguous values](#unambiguous-values)
    * [Strict spec compliance](#string-spec-compliance)
    * [Safety](#safety)
    * [Simplicity](#simplicity)
    * [Object key pooling](#object-key-pooling)
* [Dependencies](#dependencies)
* [License](#license)

# Usage

## Type Mappings

jzon maps types per the following chart:

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

## Reading

`jzon:parse` will parse JSON and produce a CL value:

``` common-lisp
(jzon:parse "
{
  \"name\": \"Rock\",
  \"coords\": {
    \"map\": \"stony_hill\",
    \"x\": 5,
    \"y\": 10
  },
  \"attributes\": [\"fast\", \"hot\"],
  \"physics\": true,
  \"item\": false,
  \"parent\": null
}")
```

`jzon:parse` reads input from its single argument and returns a parsed value per [Type Mappings](#type-mappings).

`in` can be any of the following:
* string
* (vector (unsigned-byte 8)) - octets in utf-8
* stream - character or binary in utf-8
* pathname - `parse` will open the file for reading

`jzon:parse` also accepts the follwing keyword arguments:
* `:allow-comments` This allows the given JSON to contain cpp-style `// line comments` and `/* block comments */`.
* `:allow-trailing-comma` This allows for a single trailing comma after the final element in a JSON array or object.
* `:max-depth` This controls the maximum depth to allow arrays/objects to nest. Can be a positive integer, or `nil` to disable depth tests.
* `:max-string-length` This controls the maximum length of strings. This applies for both keys and values. Must be a positive integer no larger than `array-dimension-limit`.
* `:key-fn` A function of one argument responsible for 'interning' object keys. Should accept a `simple-string` and return the 'interned' key

**Tip**: `key-fn` can be supplied as `#'identity` in order to disable [key pooling](#object-key-pooling):

``` common-lisp
(jzon:parse "[ { \"x\": 1, \"y\": 1 }, { \"x\": 1, \"y\": 1 } ]" :key-fn #'identity)
```

**Tip**: `alexandria:make-keyword` or equivalent can be used to make object keys into symbols:

``` common-lisp
(jzon:parse "[ { \"x\": 1, \"y\": 1 }, { \"x\": 1, \"y\": 1 } ]" :key-fn #'alexandria:make-keyword)
```

### Incremental/Streaming Reader

In addition to `jzon:parse`, `jzon` exposes an incremental parser for reading JSON in parts:

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
  (jzon:parse-next parser) ; => :end-object
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

As an example, `jzon:parse` could be approximately defined as follows using this API:

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

## Writing

`stringify` will serialize an object to JSON:

``` common-lisp
(jzon:stringify #("Hello, world!" 5 2.2 #(null)))
; => "[\"Hello, world!\",5,2.2,[null]]"
```

`jzon:stringify` accepts the following keyword arguments:
* `:stream` A destination like in `format`, or a `pathname`. Like `format`, returns a string if `nil`.
* `:pretty` If true, output pretty-formatted JSON
* `:coerce-element` A function for coercing 'non-native' values to JSON. See [Custom Serialization](#custom-serialization)
* `:coerce-key` A function for coercing key values to strings. See [Custom Serialization](#custom-serialization)

In addition to the mappings defined in [Type Mappings](#type-mappings), `stringify` accepts the following types of values:


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

These coercion rules only apply when using the default `:coerce-element` and `:coerce-key`.

### Symbol key case

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

### Custom Serialization

`stringify` allows serializing any values not covered in the [Type Mappings](#type-mappings) in an few different ways.

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

`stringify` recurses, so if we have:

``` common-lisp
(jzon:stringify (make-instance 'object :coordinate (make-instance 'coordinate)) :pretty t :stream t)
```

We'll have:

``` json
{
  "alive": false,
  "coordinate": {
    "x": 0,
    "y": 0
  },
  "children": []
}
```

#### coerced-fields

If you wish more control over how your object is serialized, the most straightforward way is to specialize `coerced-fields`.

Consider our previous `coordinate` class. If we always wanted to serialize only the `x` and `y` slots, and wanted to rename them, we could specialize `coerced-fields` as follows:

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

`coerced-fields` should a list of 'fields', which are two (or three) element lists of the form:

``` common-lisp
(name value &optional type)
```

The `name` can be any suitable key name. In particular, integers are allowed coerced to their decimal string representation.

The `value` can be any value - it'll be coerced if necessary.

The `type` is used as `:type` above, in order to resolve ambiguities with `nil`.

##### Including only some slots

If the default `coerced-fields` gives you most of what you want, you can exclude/rename/add fields by specializing an `:around` method as follows:

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

#### write-value
For more fine-grained control, you can specialize a method on `jzon:write-value`.

`jzon:write-value writer value`

`writer` is a [writer](#writer) on which any of the writer functions may be called to serialize your object in any desired way.


``` common-lisp
(defclass my-point () ())

(defmethod jzon:write-value (writer (value my-point))
  (jzon:write-array writer 1 2))
```

See [writer](#writer) for the available functions.

### writer

In addition to `jzon:stringify`, jzon also provides an imperative, streaming writer for writing JSON.

The following are the available functions for writing:

#### General
* `jzon:write-value` - Writes any value to the writer. Usable when writing a toplevel value, object property value, or array element.

#### Object
* `jzon:with-object`
* `jzon:begin-object`
* `jzon:write-key`
* `json:write-property`
* `json:write-properties`
* `json:end-object`
* `jzon:write-object`

#### Array
* `jzon:with-array`
* `jzon:begin-array`
* `jzon:write-values`
* `json:end-array`
* `jzon:write-array`

**Note** all functions have `*`-suffixed variants which use the `jzon:*writer*` variable, such as `jzon:write-value*`

### Example

Using the plain variants:

``` common-lisp
(let ((writer (jzon:make-writer :stream *standard-output* :pretty t)))
  (jzon:with-object writer
    (jzon:write-properties writer :age 24 "colour" "blue")
    (jzon:write-key writer 42)
    (jzon:write-value writer #(1 2 3))

    (jzon:write-key writer "an-array")
    (jzon:with-array writer
      (jzon:write-values writer :these :are :array :elements))

    (jzon:write-key writer "another array")
    (jzon:write-array writer :or "you" "can" "use these" "helpers")))
```

Using the `*` variants:
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
```

result:

``` json
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

It's worth noting that every function returns the `writer` itself for usage with arrow macros:

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

# Features

This section notes some of jzon's more noteworthy features.

In general, jzon strives for (in order):

* Safety
* Correctness
* Simplicity
* Interoperability
* Performance

## Unambiguous values

Values are never ambiguous between `[]`, `false`, `{}`, `null`, or a missing key, as in some other json parsers.

## Strict spec compliance

This parser is written against [RFC 8259][JSONRFC] and strives to adhere strictly for maximum compliance and little surprises.

Also, this has been tested against the [JSONTestSuite][JSONTestSuite]. See the [JSONTestSuite](JSONTestSuite/) directory in this repo for making & running the tests.

## Safety

[RFC 8259][JSONRFC] allows setting limits on things such as:
* Number values accepted
* Nesting level of arrays/objects
* Length of strings

jzon is meant to be safe in the face of untrusted JSON and will error on otherwise 'reasonable' input out-of-the-box.

jzon's `parse` is also type-safe, and shall not, for example:

``` common-lisp
CL-USER> (parse 2)
; Debugger entered on #<SB-SYS:MEMORY-FAULT-ERROR {1003964833}>
```
.. as in [some](https://github.com/Rudolph-Miller/jonathan) other [libraries](https://github.com/madnificent/jsown).

jzon also chooses to (by default) keep object keys as strings. This is done rather than using symbols via `intern` because over time, symbols will continue to be allocated and because they are in a package, will not be collected by the garbage collector, causing a memory leak.

## Simplicity

You call `parse`, and you get a reasonable standard CL object back.

* No custom data structures or accessors required
* No worrying about key case auto conversion or hyphens/underscores being converted.
* No worrying about what package symbols are interned in (no symbols).
* No worrying about dynamic variables affecting a parse as in cl-json, jonathan, jsown. Everything affecting `parse` is given at the call-site.

`parse` also accepts either a string, octet vector, stream, or pathname for simpler usage over libraries requiring one or the other, or having separate parse functions.

## Object key pooling

`jzon` will use a key pool per-parse, causing shared keys in a nested JSON object to share keys:

``` common-lisp
(jzon:parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]")
```
In this example, the string `x` is shared (eq) between all 3 objects.

This optimizes for the common case of reading a JSON payload containing many duplicate keys.

# Dependencies

* [closer-mop](https://github.com/pcostanza/closer-mop)
* [flexi-streams](https://github.com/edicl/flexi-streams)
* [uiop](https://gitlab.common-lisp.net/asdf/asdf)

# Alternatives

There are many CL JSON libraries available, and I defer to Sabra Crolleton's definitive list and comparisons [https://sabracrolleton.github.io/json-review](https://sabracrolleton.github.io/json-review).

But for posterity, included in this repository is a set of tests and results for the following libraries:

* cl-json
* jonathan
* json-streams
* jsown
* shasht
* yason

No ill-will is meant for these other libraries. I simply want `jzon` to be better and become a true de-facto library in the world of JSON-in-cl once and for all.

# License

See [LICENSE](LICENSE).

jzon was originally a fork of [st-json](https://marijnhaverbeke.nl/st-json/), but I ended up scrapping all of the code except for for the function decoding Unicode.

[JSONRFC]: https://tools.ietf.org/html/rfc8259
[JSONTestSuite]: https://github.com/nst/JSONTestSuite
