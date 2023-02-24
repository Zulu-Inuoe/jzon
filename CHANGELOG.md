## v1.1.0

**PENDING**

Changes relative to [v1.0.0](#v100)

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
