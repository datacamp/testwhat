#' Test result of expression
#' 
#' Test whether the given expression gives the same result in 
#' the student and the solution environment.
#' 
#' @param expr The expression that is executed in both environments.
#' @param incorrect_msg Optional feedback message in case the evaluation is not the
#' same in both environments. Automatically generated if not specified.
#' @inheritParams test_object
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # my_fun <- function(a, b) { a + b }
#' 
#' # Test whether my_fun(1,2) and my_fun(1,2)
#' # give same _result_
#' test_function_definition("my_fun", {
#'  test_expression_result(my_fun(1,2))
#'  test_expression_result(my_fun(-1,-2))
#' })
#' }
#' 
#' @export 
test_expression_result <- function(expr, 
                                   eq_condition = "equivalent",
                                   incorrect_msg = NULL) {
  ex() %>% test_expr(expr) %>% test_result() %>% test_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg)
}

#' Test output of expression
#' 
#' Test whether the given expression gives the same output in 
#' the student and the solution environment.
#' 
#' @param expr The expression that is executed in both environments.
#' @param incorrect_msg Optional feedback message in case the evaluation is not the
#' same in both environments. Automatically generated if not specified.
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # my_fun <- function(a, b) { a + b }
#' 
#' # Test whether my_fun(1,2) and my_fun(1,2)
#' # give same _output_
#' test_function_definition("my_fun", {
#'  test_expression_output(my_fun(1,2))
#'  test_expression_output(my_fun(-1,-2))
#' })
#' }
#' 
#' @export 
#' @importFrom utils capture.output
test_expression_output <- function(expr, incorrect_msg = NULL) {
  ex() %>% test_expr(expr) %>% test_output() %>% test_equal(incorrect_msg = incorrect_msg)
}

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
  ex() %>% test_expr(expr) %>% test_error(no_error_msg = no_error_msg) %>% test_equal(incorrect_msg = incorrect_msg)
}