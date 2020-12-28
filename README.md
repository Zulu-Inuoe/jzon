# jzon

A correct and safe JSON [RFC 8259][JSONRFC] parser.

[![Actions Status](https://github.com/Zulu-Inuoe/jzon/workflows/ci/badge.svg)](https://github.com/Zulu-Inuoe/jzon/actions)

# Usage

## Reading JSON

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

`parse` accepts either a string, or a stream, and returns a parsed value per [Type Mappings](#type-mappings)

`parse` accepts the follwing keyword arguments:
* `:allow-comments` This allows the given JSON to contain `//cpp-style comments`
* `:maximum-depth` This controls the maximum depth to allow arrays/objects to nest. Can be a positive integer, or `nil` to disable depth tests.

## Writing JSON

`stringify` will serialize an object to JSON:

``` common-lisp
(stringify #("Hello, world!" 5 2.2 #(null)))
; => "[\"Hello, world!\",5,2.2,[null]]"
```

`stringify` accepts the following keyword arguments:
* `:stream` A stream designator, or `nil`. if `nil`, stringify will serialize to a string and return it (as `format`)
* `:pretty` If true, output pretty-formatted JSON
* `:coerce-value` A function for coercing 'non-native' values to JSON. See [Custom Serialization](#custom-serialization)
* `:coerce-key` A function for coercing key values to strings. See [Custom Serialization](#custom-serialization)

### Custom Serialization

`stringify` allows serializing 'non-native' objects in an extensible way. Non-native means any object not in [Type Mappings](#type-mappings) in several different ways.

By default, If your object is a `standard-object`, it will be serialized as a JSON object, using each of its **bound** slots as keys.

If you wish more control over how your value is serialized, the most straightforward way is to specialize `coerced-keys`.

For example, if we had an `account` defined as follows:

``` common-lisp
(defclass account ()
  ((id :accessor account-id :initform 0)
   (name :accessor account-name :initform "")
   (token :accessor account-token)))
```

and we wanted to only serialize the `id` and `name` slots, we could specialize `coerced-keys` as follows:

``` common-lisp
(defmethod coerced-keys ((account account))
  (list (list "id" (account-id account))
        (list "name" (account-name account))))
```

This would result in:

``` json
{
  "id": 0,
  "name": "foobar"
}
```

`coerced-keys` returns a list of 'keys', which are two (or three) element lists of the form:

``` common-lisp
(name value &optional type)
```

The `value` can be any value - it'll be coerced if necessary.

The `type` is only used to resolve ambiguities with `nil`. Demonstrate, consider the following class definition:

``` common-lisp
(defclass coordinate ()
  ((x :initform 0)
   (y :initform 0)))

(defclass object ()
  ((alive
    :initform nil
    :type boolean)
   (coordinate
    :initform nil
    :type (or null coordinate))
   (children
    :initform nil
    :type list)))
```

By default, a fresh instance of this object shall be serialized as:

``` json
{
  "ALIVE": false,
  "COORDINATE": null,
  "CHILDREN": []
}
```

Notice we have three distinct interpretations of `nil` - boolean, null, and an empty array.

This works by making use of the `:type` annotation on the slot.

A specialization of `coerced-keys` can return provide any valid `type` and inherit this behaviour.

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

# Features

These are some of jzon's specific features.

## Object key pooling

`jzon` will use a key pool per-parse, causing shared keys in a nested JSON object to share keys:

``` common-lisp
(parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]")
```
In this example, the string `x` is shared (eq) between all 3 objects.

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

`parse` also accepts either a string, or a stream for simpler usage over libraries requiring one or the other, or having separate parse functions.

# License

See [LICENSE](LICENSE).

jzon was originally a fork of [st-json](https://marijnhaverbeke.nl/st-json/), but I ended up scrapping all of the code except for for the function decoding Unicode.

[JSONRFC]: https://tools.ietf.org/html/rfc8259
[JSONTestSuite]: https://github.com/nst/JSONTestSuite
