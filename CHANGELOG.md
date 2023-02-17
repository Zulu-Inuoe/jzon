## v1.1.0

**PENDING**

Changes relative to [v1.0.0](#v100)

* Fix Clozure Common Lisp, and LispWorks support https://github.com/Zulu-Inuoe/jzon/issues/27
* Add new `jzon:json-limit-error` conditions so users can discriminate between invalid JSON, and JSON that exceeds set limits
* Signal `jzon:json-write-error` rather than `error` when issuing invalid commands to `jzon:writer`
* Resolve issues around `max-string-length` and introduce the ability to choose default `t` and 'no limit' `nil`

## v1.0.0

Initial Release

:tada:
