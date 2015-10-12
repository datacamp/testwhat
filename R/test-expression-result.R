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
#' test_function_defintion({
#'  test_expression_result(my_fun(1,2))
#'  test_expression_result(my_fun(-1,-2))
#' })
#' }
#' 
#' @import datacampAPI
#' @import testthat
#' @export 
test_expression_result <- function(expr, 
                                   eq_condition = "equivalent",
                                   student_env = .GlobalEnv,
                                   solution_env = get_solution_env(),
                                   incorrect_msg = NULL) {
  
  capture.output(result_sol <- try(eval(parse(text = expr), envir = solution_env), silent = TRUE))
  if (length(result_sol) == 0) {
    result_sol <- "NULL"
  }
  
  if (inherits(result_sol, "try-error")) {
    stop("expr in test_result() results in an error in the solution environment")
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Make sure that running <code>%s</code> results in <code>%s</code>", expr, build_summary(result_sol))
  }
  
  capture.output(result_stud <- try(eval(parse(text = expr), envir = student_env), silent = TRUE))
  if (length(result_stud) == 0) {
    result_stud <- "NULL"
  }
  
  if (inherits(result_stud, "try-error")) {
    test_what(fail(), 
              sprintf("%s<br>Instead, it resulted in the following error: <i>%s</i>", 
                      incorrect_msg, 
                      build_summary(attr(result_stud,"condition")$message, output = TRUE)))
  } else {
    eq_fun <- switch(eq_condition,
                     equivalent = expect_equivalent,
                     equal = expect_equal,
                     identical = expect_identical,
                     stop("invalid equality condition"))

    test_what(eq_fun(result_sol, result_stud),
              sprintf("%s<br>Instead, got: <code>%s</code>", 
                      incorrect_msg, 
                      build_summary(result_stud)))
  }
}