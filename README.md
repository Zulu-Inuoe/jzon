# jzon

A correct and safe JSON [RFC 8259](https://tools.ietf.org/html/rfc8259) parser.

# Usage

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

`parse` accepts either a string, or a stream, and returns a parsed object depending on the following chart:


| JSON   | CL                      |
|--------|-------------------------|
| true   | symbol `t`              |
| false  | symbol `nil`            |
| null   | symbol `null`           |
| number | integer or double-float |
| string | simple-string           |
| array  | simple-vector           |
| object | hash-table (equal)      |

`parse` accepts the follwing keyword arguments:
* `:allow-comments` This allows the given JSON to contain `//cpp-style comments`
* `:maximum-depth` This controls the maximum depth to allow arrays/objects to nest. Can be a positive integer, or `nil` to disable depth tests.

# Features

## Object key pooling

`jzon` will use a key pool per-parse, causing shared keys in a nested JSON object to share keys:

``` common-lisp
(parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]")
```
In this example, the string `x` is shared (eq) between all 3 objects.

## Unambiguous valus

Values are never ambiguous between `[]`, `false`, `{}`, `null`, as in some other json parsers.

## Strict spec compliance
This parser is written against [RFC 8259](https://tools.ietf.org/html/rfc8259) and strives to adhere strictly for maximum compliance and little surprises.

Also, this has been tested against the [JSONTestSuite](https://github.com/nst/JSONTestSuite). See the [JSONTestSuite](JSONTestSuite/) directory in this repo for making & running the tests.

# License
See [LICENSE](LICENSE).

jzon was originally a fork of [st-json](https://marijnhaverbeke.nl/st-json/), but I ended up scrapping all of the code except for for the function decoding Unicode.
