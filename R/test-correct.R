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
#'  is a fail in the end, the tests in \code{check_code} are run afterwards,
#'  this time 'loudly'.}
#' }
#' 
#' \code{test_correct} reduces computation time (if it's ok, the additional 
#' battery of tests is not run) and increases the flexibility for the student 
#' (if the final result is ok, different paths towards this result are allowed).
#' 
#' @param check_code High-level tests. Also provide feedback messages here, 
#' as this code is run loudly after executing the \code{diagnose_code} code, 
#' in the case of failing tests.
#' @param diagnose_code Low-level tests that are run if tests in \code{check_code} fail.
#' @param env environment in which to execute tests.
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # x <- mean(1:3, na.rm = TRUE)
#' 
#' # Example SCT
#' test_correct({
#'  test_object("x")
#' }, {
#'  # this code only is run if test_object("x") fails
#'  test_function("mean", "x")
#'  # if test_function passes, test_object("x") is 
#'  # automatically run again to generate a fail.
#' })
#' }
#'
#' @export
test_correct = function(check_code, diagnose_code, env = parent.frame()) {
  check_code <- substitute(check_code)
  if (is.character(check_code)) check_code <- parse(text = check_code)
  
  diagnose_code <- substitute(diagnose_code)
  if (is.character(diagnose_code)) diagnose_code <- parse(text = diagnose_code)
  
  rep <- get_reporter()
  
  rep$be_silent()
  eval(check_code, envir = env)
  rep$be_loud()
  
  # If the check_code failed, do more tests
  if(rep$silent_fail) {
    eval(diagnose_code, envir = env)
    # test the check one more, now loudly
    eval(check_code, envir = env)
  }
}