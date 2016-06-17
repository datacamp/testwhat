#' Test error of expression
#' 
#' Test whether the given expression throws an error
#' 
#' @param expr The expression that is executed in both environments.
#' @param no_error_msg Optional feedback message when the expression does not
#' generate an error when run in the student's environment.
#' @param incorrect_msg Optional feedback message in case the evaluation is not the
#' same in both environments. Automatically generated if not specified.
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # my_fun <- function(a) { stopifnot(a > 0) }
#' 
#' # Test whether my_fun(-1) throws error
#' test_function_definition("my_fun", {
#'  test_expression_error(my_fun(-1))
#' })
#' }
#' 
#' @export 
test_expression_error <- function(expr, no_error_msg = NULL, incorrect_msg = NULL) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  init_tags(fun = "test_expression_output")
  
  if (is.null(no_error_msg)) {
    no_error_msg <- sprintf("It seems that running <code>%s</code> does not generate an error while it should", expr)
  }
  

  result_sol <- try(eval(parse(text = expr), envir = solution_env), silent = TRUE)
  if (!inherits(result_sol, "try-error")) {
    stop(sprintf("%s does not generate an error when run in the solution environment.", expr))
  }
  sol_error <- attr(result_sol, "condition")$message
  
  if (is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Make sure that running <code>%s</code> generates the following error:<br><code>%s</code>", expr, sol_error)
  }
  
  result_stud <- try(eval(parse(text = expr), envir = student_env), silent = TRUE)
  if (!inherits(result_sol, "try-error")) {
    stop(sprintf("%s does not generate an error"))  
  }
  
  test_what(expect_true(inherits(result_stud, "try-error")), feedback = list(message = no_error_msg))
  
  student_error <- attr(result_stud, "condition")$message
  test_what(expect_equal(student_error, sol_error), feedback = list(message = incorrect_msg))
}