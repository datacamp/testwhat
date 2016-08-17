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
#' @export
test_correct <- function(check_code, diagnose_code) {
  rep <- get_rep()
  check_passed <- run_until_fail(substitute(check_code))
  check_feedback <- rep$get_feedback()
  diagnose_passed <- run_until_fail(substitute(diagnose_code))
  diagnose_feedback <- rep$get_feedback()
  if (check_passed) {
    # all good
  } else {
    if (diagnose_passed) {
      check_that(failure(), feedback = check_feedback)
    } else {
      check_that(failure(), feedback = diagnose_feedback)
    }
  }
}

#' Test if one of the given sct parts are correct. 
#' 
#' Test if one of the given SCT code batteries are evaluated as being
#' correct. If not, the feedback message of the first fail is standardly
#' given. Can be used nested.
#' 
#' \itemize{
#'  \item{\code{...}: an arbritrary amount of code blocks containing SCT 
#'  code. \code{test_or} will check if one of the code blocks results in 
#'  a successful SCT evaluation.}
#' }

#' @param ... one of these code blocks with tests should succeed 
#' @param incorrect_msg msg displayed when none succeeds
#' @param choose_feedback choose feedback of test with this index
#' 
#' @examples
#' \dontrun{
#'   # test if either the object a or the object b is correct
#'   test_or(test_object("a"), test_object("b"))
#' }
#'
#' @export
test_or <- function(..., incorrect_msg = NULL, choose_feedback = 1) {
  test_env <- ex()$get("test_env")
  
  input <- substitute(alist(...))
  input[[1]] <- NULL
  
  len <- length(input) 
  
  passes <- logical(len)
  feedback <- list()
  rep <- get_rep()
  
  for (i in seq_along(input)) {
    code <- input[[i]]
    passes[i] <- run_until_fail(code)
    feedback[[i]] <- rep$get_feedback()
  }
  
  if (!any(passes)) {
    if (is.null(incorrect_msg)) {
      check_that(failure(), feedback = feedback[[choose_feedback]])
    } else {
      check_that(failure(), feedback = incorrect_msg)
    }
  }
}