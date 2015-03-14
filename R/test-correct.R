#' Test if a set of tests passes, if not, do additional set of tests
#' 
#' Test if a set of tests passes, and do additional, more precise tests if there were failures.
#' The teacher should specify two code chunks; \code{check_code}, that specifies the code that 
#' checks on the (typically, final results of the) student's code. These tests are executed silently,
#' without the reporter generating information for these. If these 'check tests' fail, \code{test_correct}
#' runs another set of tests, specifies in \code{diagnose_code}. This set of tests contains more detailed
#' checks on the student's code, to pinpoint the problem. Afterwards, the code specified in \code{check_code} is run again.
#' As such, \code{test_correct} reduces computation time (if it's ok, the additional battery of tests is not run) and
#' increases the flexibility for the student (if the final result is ok, different paths towards this result are allowed).
#' 
#' @param check_code Battery of high-level tests. Also provide feedback messages here, as this code is run loudly
#' after executing the \code{diagnose_code} code, in the case of failing tests.
#' @param diagnose_code Battery of more low-level tests to pinpoint the problem in a student's code, 
#' in case the check_code generated failures. Here, the feedback is recorded.
#' @param env environment in which to execute tests.
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