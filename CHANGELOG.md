## v1.1.0

Changes relative to [v1.0.0](#v100)

* [ECL][ecl] Support https://github.com/Zulu-Inuoe/jzon/issues/36
* Add `jzon:parse-next-element` utility function for parsing a full element using the streaming reader.
* bugfix - signal `jzon:json-eof-error` when we encounter an incomplete unicode escape sequence such as `\uAD`. Used to signal `cl:type-error`.
* bugfix - `jzon:parse-next` no longer returns 3 values on `:object-key`
* Add `jzon:span` for easier subsequence support for `jzon:parse` and `jzon:parser` https://github.com/Zulu-Inuoe/jzon/issues/30
* `jzon:parse-next` no longer 'over-reads' strings, objects, and arrays.
* `allow-multiple-content` in reader functions now prevents jzon from scanning content after the toplevel object to signal error. Can be used to support formats like [JSON Lines][json-lines].
* faster parsing from (vector (unsigned-byte 8)) and binary streams https://github.com/Zulu-Inuoe/jzon/issues/29
* bugfix - `allow-trailing-comma` was not being properly applied when reading from vectors or pathnames
* bugfix - `max-string-length` was not being properly applied when reading from vectors or pathnames
* `max-depth` in reader/writer functions can now be set to `t` to indicate 'default'
* Fix Clozure Common Lisp, and LispWorks support https://github.com/Zulu-Inuoe/jzon/issues/27
* Add new `jzon:json-limit-error` conditions so users can discriminate between invalid JSON, and JSON that exceeds set limits
* Signal `jzon:json-write-error` rather than `error` when issuing invalid commands to `jzon:writer`
* Resolve issues around `max-string-length` and introduce the ability to choose default `t` and 'no limit' `nil`

Incompatible changes relative to [v1.0.0](#v100):

* `:key-fn` being `nil` now disables pooling, and `t` enables the default pool for `jzon:parse` and `jzon:make-parser`. This is for consistency with other arguments.

## v1.0.0

Initial Release

:tada:

[json-lines]: https://jsonlines.org/
[ecl]: https://gitlab.com/embeddable-common-lisp/ecl
