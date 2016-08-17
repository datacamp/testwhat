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
                                     incorrect_number_arguments_msg = NULL) {
  
}

#' @export
test_fun_def <- function(state, name, undefined_msg = NULL, no_fundef_msg = NULL) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  fundef_state <- FunDefState$new(state)
  fundef_state$add_details(type = "fundef",
                          case = "defined",
                          name = name,
                          message = undefined_msg,
                          pd = NULL)
  
  check_defined(name, solution_env)
  solution_object <- get(name, envir = solution_env, inherits = FALSE)
  if (!inherits(solution_object, "function")) {
    stop("%s is not a user-defined function in the solution code.", name)
  }
  
  check_that(is_true(exists(name, envir = student_env, inherits = FALSE)), feedback = fundef_state$details)
  student_object <- get(name, envir = student_env, inherits = FALSE)
  
  fundef_state$set_details(case = "correcttype",
                           message = no_fundef_msg
                           #pd = extract_function_definition(state$get("student_pd"), name))
                           )
  
  check_that(is_true(inherits(student_object, "function")),
             feedback = fundef_state$details)
  
  fundef_state$set_details(case = "correct",
                           message = NULL)
  
  fundef_state$set(name = name,
                   student_object = student_object,
                   solution_object = solution_object)
  
  return(fundef_state)
}

test_arguments <- function(state, incorrect_number_args_msg = NULL) {
  stud_arguments <- as.list(formals(state$get("student_object")))
  sol_arguments <- as.list(formals(state$get("solution_object")))
  
  fundefargs_state <- FunDefArgsState$new(state)
  
  fundefargs_state$add_details(type = "fundef",
                               case = "arguments",
                               message = incorrect_number_args_msg,
                               pd = NULL)
  
  check_that(is_equal(length(stud_arguments), length(sol_arguments)), 
             feedback = fundefargs_state$details)
  return(fundefargs_state)
}

#' @export
test_body.FunDefState <- function(state, not_found_msg = NULL) {
  name <- state$get("name")
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  body_state <- SubState$new(state)
  
  body_state$add_details(type = "fundef",
                         case = "coded",
                         message = not_found_msg,
                         pd = NULL)
  
  solution_fun_def <- extract_function_definition(solution_pd, name)
  if (is.null(solution_fun_def)) {
    stop(sprintf("The function definition of %s was not found in the solution code", name))
  }
  
  student_fun_def <- extract_function_definition(student_pd, name)
  check_that(is_false(is.null(student_fun_def)), feedback = body_state$details)
  
  body_state$set_details(type = "body", pd = student_fun_def$pd)
  decorate_state(body_state, student_fun_def, solution_fun_def)
  return(body_state)
}


#' @export
test_result.FunDefState <- function(state, ..., error_msg = NULL) {
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  run_expr_helper(state, 
                  expr = parse(text = expr_str),
                  expr_str = expr_str,
                  error_msg = error_msg,
                  case = "result")
}

#' @export
test_output.FunDefState <- function(state, ..., error_msg = NULL) {
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  run_expr_helper(state, 
                  expr = parse(text = expr_str),
                  expr_str = expr_str,
                  error_msg = error_msg,
                  case = "output")
}

#' @export
test_error.FunDefState <- function(state, ..., no_error_msg = NULL) {
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  run_expr_error_helper(state, 
                        expr = parse(text = expr_str),
                        expr_str = expr_str,
                        no_error_msg = no_error_msg)
}





