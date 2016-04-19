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

#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # my_fun <- function(a, b) { a + b }
#' 
#' # SCT testing both result and printouts:
#' test_function_definition({
#'  test_expression_result(my_fun(1,2))
#'  test_expression_output(my_fun(1,2))
#' }, {
#'  test_student_typed("+")
#' })
#' }
#' 
#' @export
test_function_definition <- function(name, 
                                     function_test = NULL, 
                                     body_test = NULL,
                                     undefined_msg = NULL, 
                                     incorrect_number_arguments_msg = NULL,
                                     env = parent.frame()) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  fun_usage <- tw$get("fun_usage")
  init_tags(fun = "test_function_definition")
  
  on.exit({
    tw$set(student_pd = student_pd)
    tw$set(solution_pd = solution_pd)
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
    tw$set(fun_usage = fun_usage)
  })
  
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
  if (!exists(name, solution_env)) {
    stop(sprintf("%s is not defined in your solution environment.", name))
  }
  
  sol_function <- get(name, envir = solution_env, inherits = FALSE)

  function_test <- substitute(function_test)
  body_test <- substitute(body_test)
  
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Did you define the function <code>%s()</code>?", name)
  }

  if (is.null(incorrect_number_arguments_msg)) {
    incorrect_number_arguments_msg <- sprintf("Did you specify the correct number of arguments in the function <code>%s()</code>?", name)
  }
  
  defined <- exists(name, envir = student_env, inherits = FALSE)
  test_what(expect_true(defined), undefined_msg)
  
  rep <- get_reporter()
  rep$be_silent()
  passes <- run_until_fail(function_test, env = student_env)
  rep$be_loud()
  
  if (!passes) {
    stud_function <- get(name, envir = student_env, inherits = FALSE)
    stud_arguments <- as.list(formals(stud_function))
    sol_arguments <- as.list(formals(sol_function))
    
    test_what(expect_equal(length(stud_arguments), length(sol_arguments)), incorrect_number_arguments_msg)
    
    
    solution_fun_def <- extract_function_definition(solution_pd, name)
    student_fun_def <- extract_function_definition(student_pd, name)
    
    if (is.null(solution_fun_def)) {
      stop(sprintf("The function definition if %s was not found in the solution code", name))
    }
    test_what(expect_false(is.null(student_fun_def)), 
              feedback = sprintf("A proper definition of `%s` could not be found in your submission. Make sure to use the `%s <- function() { ... }` recipe.", name, name))
    
    if (!is.null(body_test)) {
      tw$set(student_pd = student_fun_def$pd)
      tw$set(solution_pd = solution_fun_def$pd)
      tw$set(student_code = student_fun_def$code)
      tw$set(solution_code = solution_fun_def$code)
      eval(body_test, envir = env)
    }
    
    eval(function_test, envir = env)
  }
}