# Changelog

All notable changes to the `testwhat` project will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 4.4.3

### Added

- `check_library()`: `check_` equivalent for `test_library_function()`. Documentation has been udpated accordingly
- `check_code()` now takes a `drop_comments` argument. It defaults to `FALSE`. If you set it to `TRUE`, comments will be stripped from the student submission before the pattern you specified (fixed or not) is sought.

### Changed

- `check_library()` (and by extension `test_library_function()`) now ignore `library()` and `require()` calls in comments.
