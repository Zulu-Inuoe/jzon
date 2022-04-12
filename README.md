# jzon

A correct and safe JSON [RFC 8259][JSONRFC] parser.

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

There's a single entry point: `parse`:

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

`parse` reads input from its single argument and returns a parsed value per [Type Mappings](#type-mappings).

`in` can be any of the following:
* string
* (vector (unsigned-byte 8)) - octets in utf-8
* stream - character or binary in utf-8
* pathname - `parse` will open the file for reading

`parse` also accepts the follwing keyword arguments:
* `:allow-comments` This allows the given JSON to contain `//cpp-style comments`
* `:max-depth` This controls the maximum depth to allow arrays/objects to nest. Can be a positive integer, or `nil` to disable depth tests.
* `:max-string-length` This controls the maximum length of strings. This applies for both keys and values. Must be a positive integer no larger than `array-dimension-limit`.
* `:key-fn` A function of one argument responsible for 'interning' object keys. Should accept a `simple-string` and return the 'interned' key

**Tip**: `key-fn` can be supplied as `#'identity` in order to disable [key pooling](#object-key-pooling):

``` common-lisp
(parse "[ { \"x\": 1, \"y\": 1 }, { \"x\": 1, \"y\": 1 } ]" :key-fn #'identity)
```

**Tip**: `alexandria:make-keyword` or equivalent can be used to make object keys into symbols:

``` common-lisp
(jzon:parse "[ { \"x\": 1, \"y\": 1 }, { \"x\": 1, \"y\": 1 } ]" :key-fn #'alexandria:make-keyword)
```

## Writing

`stringify` will serialize an object to JSON:

``` common-lisp
(stringify #("Hello, world!" 5 2.2 #(null)))
; => "[\"Hello, world!\",5,2.2,[null]]"
```

`stringify` accepts the following keyword arguments:
* `:stream` A stream designator, or `nil`. if `nil`, stringify will serialize to a string and return it (as `format`)
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
(let ((ht (make-hash-table)))
  (setf (gethash 'all-upper ht) 0)
  (setf (gethash '|mixedCase| ht) 0)

  (stringify ht))
```

shall result in:

``` json
{
  "all-upper": 0,
  "mixedCase": 0
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

If we stringify a fresh `coordinate` object via `(stringify (make-instance 'coordinate))`, we'd end up with:

``` json
{
  "x": 0,
  "y": 0
}
```

And if we `(stringify (make-instance 'coordinate :reference "Earth"))`:

``` json
{
  "reference": "Earth",
  "x": 0,
  "y": 0
}
```

Similarly if we `(stringify (make-instance 'object))`:

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
(stringify (make-instance 'object :coordinate (make-instance 'coordinate)))
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
(defmethod coerced-fields ((coordinate coordinate))
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
(defmethod coerced-fields :around ((coordinate coordinate))
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

Additionally, because jzon uses strings for object keys, rather than symbols, there is no risk of running out of memory from unbounded symbol interning over multiple parses.

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
(parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]")
```
In this example, the string `x` is shared (eq) between all 3 objects.

This optimizes for the common case of reading a JSON payload containing many duplicate keys.

# Dependencies

* [closer-mop](https://github.com/pcostanza/closer-mop)
* [flexi-streams](https://github.com/edicl/flexi-streams)

# License

See [LICENSE](LICENSE).

jzon was originally a fork of [st-json](https://marijnhaverbeke.nl/st-json/), but I ended up scrapping all of the code except for for the function decoding Unicode.

[JSONRFC]: https://tools.ietf.org/html/rfc8259
[JSONTestSuite]: https://github.com/nst/JSONTestSuite
