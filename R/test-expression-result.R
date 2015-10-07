#' Test result of expression
#' 
#' Test whether the given expression gives the same result in 
#' the student and the solution environment.
#' 
#' @param expr The expression that is executed in both environments.
#' @param eq_condition Character vector indicating how to perform the
#' comparison of the results. See \code{\link{test_object}}.
#' @param student_env environment in which the student's code was evaluated.
#' @param solution_env environment in which the solution code was evaluated.
#' @param incorrect_msg Optional feedback message in case the evaluation is not the
#' same in both environments. Automatically generated if not specified.
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
#' @export 
test_expression_result <- function(expr, 
                                   eq_condition = "equivalent",
                                   student_env = .GlobalEnv,
                                   solution_env = get_solution_env(),
                                   incorrect_msg = NULL) {
  
  capture.output(result_sol <- try(eval(parse(text = expr), envir = solution_env), silent = TRUE))
  
  if (inherits(result_sol, "try-error")) {
    stop("expr in test_result() results in an error in the solution environment")
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Make sure that running <code>%s</code> results in <code>%s</code>.", expr, result_sol)
  }
  
  capture.output(result_stud <- try(eval(parse(text = expr), envir = student_env), silent = TRUE))
  
  if (inherits(result_stud, "try-error")) {
    test_what(fail(), 
              sprintf("%s<br>Instead, it resulted in the following error: <i>%s</i>", 
                      incorrect_msg, 
                      attr(result_stud,"condition")$message))
  } else {
    eq_fun <- switch(eq_condition,
                     equivalent = expect_equivalent,
                     equal = expect_equal,
                     identical = expect_identical,
                     stop("invalid equality condition"))
    
    test_what(eq_fun(result_sol, result_stud),
              sprintf("%s<br>Instead, got: <code>%s</code>", 
                      incorrect_msg, 
                      result_stud))
  }
}