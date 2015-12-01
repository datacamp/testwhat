#' Check whether the student defined a function correctly
#' 
#' @param name  The name of the function to test
#' @param function_test tests to perform on the function
#' (use \code{\link{test_expression_output}} and \code{\link{test_expression_result}}).
#' @param body_test Additional tests to perform on the body of the function if the tests
#' in \code{function_test} fail. Only able to test on strings here!
#' @param undefined_msg Optional feedback message in case the specified 
#' function was not defined
#' @param incorrect_number_arguments_msg Optional feedback message in case the 
#' function does not have the correct number of arguments.
#' @param env Environment in which to perform the tests
#' @inheritParams test_object
#' @inheritParams test_function

#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # my_fun <- function(a, b) { a + b }
#' 
#' # SCT testing both result and printouts:
#' test_function_defintion({
#'  test_expression_result(my_fun(1,2))
#'  test_expression_output(my_fun(1,2))
#' }, {
#'  test_student_typed("+")
#' })
#' }
#' 
#' @import datacampAPI
#' @import testthat
#' @export
test_function_definition <- function(name, 
                                     function_test = NULL, 
                                     body_test = NULL,
                                     student_env = .GlobalEnv,
                                     solution_env = get_solution_env(),
                                     student_code = get_student_code(), 
                                     solution_code = get_solution_code(),
                                     undefined_msg = NULL, 
                                     incorrect_number_arguments_msg = NULL,
                                     env = parent.frame()) {
  
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
  if (!exists(name, solution_env)) {
    stop(sprintf("%s is not defined in your solution environment.", name))
  }
  
  sol_function <- get(name, envir = solution_env, inherits = FALSE)

  function_test <- substitute(function_test)
  if (is.character(function_test)) code <- parse(text = function_test)
  
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Did you define the function <code>%s()</code>?", name)
  }

  if (is.null(incorrect_number_arguments_msg)) {
    incorrect_number_arguments_msg <- sprintf("Did you specify the correct number of arguments in the function <code>%s()</code>?", name)
  }
  
  defined <- exists(name, envir = student_env, inherits = FALSE)
  test_what(expect_true(defined), undefined_msg)
  
  if (defined) {
    stud_function <- get(name, envir = student_env, inherits = FALSE)
    stud_arguments <- as.list(formals(stud_function))
    sol_arguments <- as.list(formals(sol_function))
    
    rep <- get_reporter()
    
    rep$be_silent()
    eval(function_test, envir = student_env)
    fail <- rep$get_silent_fail()
    rep$be_loud()
    
    if (fail) {
      test_what(expect_equal(length(stud_arguments), length(sol_arguments)), incorrect_number_arguments_msg)
      
      if(!is.null(body_test)) {
        set_student_code(paste(deparse(stud_function), collapse = "\n"))
        set_solution_code(paste(deparse(sol_function), collapse = "\n"))
        eval(body_test, envir = env)
        set_student_code(student_code)
        set_solution_code(solution_code)  
      }
      
      eval(function_test, envir = student_env)
    }
  }
}