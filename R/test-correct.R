#' Test things. If it fails, test additional things.
#' 
#' Test if a set of tests passes, and do additional, more precise 
#' tests if there were failures. The teacher should specify two code chunks; 
#' \itemize{
#'  \item{\code{check_code}: specifies the code that checks on the 
#'  (typically, final results of the) student's code. These tests are executed 
#'  silently, without the reporter generating information for these.}
#'  \item{\code{diagnose_code}: Set of tests that gets executed if the 
#'  tests in \code{check_code} fail. These tests contain more 
#'  detailed tests, to pinpoint the problem. To make sure there 
#'  is a fail in the end, the tests in \code{check_code} are run again, 
#'  this time not silently (so they can actually cause a message).}
#' }
#' 
#' \code{test_correct} reduces computation time (if it's ok, the additional 
#' battery of tests is not run) and increases the flexibility for the student 
#' (if the final result is ok, different paths towards this result are allowed).
#' 
#' @param check_code high-level tests. Also provide feedback messages here, 
#' as this code is run loudly after executing the \code{diagnose_code} code, 
#' in the case of failing tests.
#' @param diagnose_code low-level tests that are run if tests in \code{check_code} fail.
#' @param env environment in which to execute tests.
#' 
#' @examples
#' \dontrun{
#' # Example solution code:
#' # x <- mean(1:3, na.rm = TRUE)
#' 
#' # Example SCT
#' test_correct({
#'  test_object("x")
#' }, {
#'  # this code only is run if test_object("x") fails
#'  test_function("mean", "x")
#'  # test_object("x") is automatically run again to generate a fail if test_function passed.
#' })
#' }
#'
#' @export
test_correct <- function(check_code, diagnose_code, env = parent.frame()) {
  in_test_mode <- tw$get("in_test_mode")
  check_code <- substitute(check_code)
  diagnose_code <- substitute(diagnose_code)
  rep <- get_reporter()
  rep$be_silent()
  ok <- run_until_fail(check_code, env = env)
  
  if (!ok || in_test_mode) {
    rep$be_loud()
    eval(diagnose_code, envir = env)
    eval(check_code, envir = env)
  } else {
    # Execute this part to deal with 'blacklisting' of function calls
    run_until_fail(diagnose_code, env = env)
    rep$be_loud()
  }
}