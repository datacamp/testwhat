# Changelog

All notable changes to the `testwhat` project will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 4.10.0

- Add optional parameter to force passing the `diagnose` tests in `check_correct`.

## 4.9.8

### Fixed

- Blacklisting removal introduced a bug that is now fixed.
- Checking a function call zooms in on the code for chained checking.

## 4.9.0

### Changed

- Blacklisting has been removed as a mechanism to help match function calls.

## 4.8.0

### Added

- Automatic `ex() %>% check_error()` check after an SCT finished successfully, unless `allow_errors` is set to `FALSE` by RBackend, which is the case if the exericse has `allow_solution_error()` in the pre-exercise-code
- Explicitly export `disable_highlighting()`
- Add example for test to check for function definitions

### Fixed

- Some typos in error messages (thanks Richie)

## 4.7.3

### Improved

- Error out in case the first argument to any `check_` function is not a state, which happens if you forget the `.`.
- Error out if the code you pass in `override_solution()` does not parse.
- Improve test coverage.

## 4.7.2

### Improved

- Improve check_or/check_correct message if it is used incorrectly

### Added

- Error out in case `check_object()` is not used on root state (and `TESTWHAT_V2_ONLY` set)
- Error out in case `check_output_expr()` is not used on root state
- Error out in case `check_function()` called on `check_object()`
- Error out in case `check_call()` not called on `check_function_def()`
- Error out in case `check_arg()` not called on `check_function()`

## 4.7.1

### Added

- New function `check_mc()` to use instead of `test_mc()`.

### Changed

- If `TESTWHAT_V2_ONLY=1` the `test_mc()` function will error out.
- `test_correct()`, `test_or()` will error out.
- If `TESTWHAT_V2_ONLY=1`, old skool usage of `check_correct()` and `check_or()` will error out.
  You _have_ to pipe in `ex()`:

  ```R
  ex() %>% check_correct(...)
  ex() %>% check_or(...)
  ```

## 4.7.0

When executing old skool `test_()` functions, `testwhat` will now check the environment variable `TESTWHAT_V2_ONLY`. If it is set to `1`, an error will be generated. This will effectively make `test_()` functions unavailable for newer courses whose course image will have this environment variable baked in.

## 4.6.0

### Added

- You can now pipe the state into `check_correct()` and `check_or()`. These functions will then expose a special variable `.` for the subtests to use:

  ```R
  ex() %>% check_correct(
    check_object(., 'x') %>% check_equal(),
    check_function(., 'mean') %>% check_arg('x') %>% check_equal()
  )
  ```

- You can specify custom equality functions to compare objects, function arguments and function results:

  ```R
  # solution
  x <- list(a = 1)

  # sct
  ex() %>% check_object('x') %>% check_equal(eq_fun = function(x, y) { x$a == x$b })

  # submissions that will pass
  x <- list(a = 1)
  x <- data.frame(a = 1)
  x <- list(a = 1, b = 2)
  ```

## 4.5.1 - slight markdown improvements

### Added

- Support to test `h1`-level tags (starting with `#` or underlined with `=====`)

### Changed

- Automated messaging around `check_header()` does not use the `h<num>` reference, instead uses `level <num>` reference.

## 4.5.0 - test markdown exercises, the tidy way

### Added

- Several functions to verify the correctness of exercises of the type `MarkdownExercise`, such as `check_rmd()`, `check_yaml()`, `check_header()`, `check_chunk()` and `check_option()`. Together, they allow for a gradual 'zooming in' on particular parts of the markdown file submitted by the student and the markdown file contained in the solution.
- `disable_highlighting()`: when used in an `%>%`-based SCT chain, this disables highlighting for any checking functions that follow. If there is highlighting information for before the `disable_highlighting()` function, this will be used as a fallback.
- Handle for integrating `testwhat` as a grader into `learnr`.

### Changed

- Several improvements to documentation and vignettes overall.

### Removed

- Support for `allow_extra` argument in `test_yaml_header()` and `test_chunk_options()`, as they were used nowhere on the platform. These functions will soon be either removed or updated to depend on the new tidy markdown testing functions.

## 4.4.3

### Added

- `check_library()`: `check_` equivalent for `test_library_function()`. Documentation has been udpated accordingly
- `check_code()` now takes a `drop_comments` argument. It defaults to `FALSE`. If you set it to `TRUE`, comments will be stripped from the student submission before the pattern you specified (fixed or not) is sought.

### Changed

- `check_library()` (and by extension `test_library_function()`) now ignore `library()` and `require()` calls in comments.
