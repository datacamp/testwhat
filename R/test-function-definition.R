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


fundefcall_helper <- function(state, ..., error_msg = NULL, type = c("result", "output")) {
  type <- match.arg(type)
  converter <- switch(type, result = force, output = capture.output)
  statetype <- switch(type, result = FunDefResultState, output = FunDefOutputState)
  
  name <- state$get("name")
  funcallstr <- gsub("list", name, deparse(substitute(list(...))))
  
  fundefcall_state <- statetype$new(state)
  fundefcall_state$add_details(type = "fundef",
                                 case = sprintf("%s_runs", type),
                                 callstr = funcallstr,
                                 message = error_msg,
                                 pd = NULL)
  
  sol_res <- tryCatch(converter(do.call(name, list(...), envir = state$get("solution_env"))), error = function(e) e)
  if (inherits(sol_res, 'error')) {
    stop(sprintf("Running %s gave an error", funcallstr))
  }
  
  stud_res <- tryCatch(converter(do.call(name, list(...), envir = state$get("student_env"))), error = function(e) e)
  check_that(is_false(inherits(stud_res, 'error')), feedback = fundefcall_state$details)
  
  fundefcall_state$set_details(type = "fundef",
                                 case = sprintf("%s_correct", type),
                                 message = NULL,
                                 pd = NULL)
  fundefcall_state$set(student_object = stud_res, solution_object = sol_res)
  return(fundefcall_state)
}


#' @export
test_result <- function(state, ..., error_msg = NULL) {
  fundefcall_helper(state, ..., error_msg = error_msg, type = "result")
}

#' @export
test_output <- function(state, ..., error_msg = NULL) {
  fundefcall_helper(state, ..., error_msg = error_msg, type = "output")
}

#' @export
test_error <- function(state, ..., no_error_msg = NULL) {
  name <- state$get("name")
  funcallstr <- gsub("list", name, deparse(substitute(list(...))))
  
  fundeferror_state <- FunDefErrorState$new(state)
  fundeferror_state$add_details(type = "fundef",
                               case = "error_fails",
                               callstr = funcallstr,
                               message = no_error_msg,
                               pd = NULL)
  
  sol_res <- tryCatch(do.call(name, list(...), envir = state$get("solution_env")), error = function(e) e)
  if (!inherits(sol_res, 'error')) {
    stop(sprintf("Running %s didn't give an error, while it should.", funcallstr))
  }
  
  stud_res <- tryCatch(do.call(name, list(...), envir = state$get("student_env")), error = function(e) e)
  check_that(is_true(inherits(stud_res, 'error')), feedback = fundeferror_state$details)
  
  fundeferror_state$set_details(type = "fundef",
                               case = "error_correct",
                               message = NULL,
                               pd = NULL)
  fundeferror_state$set(student_object = stud_res$message, solution_object = sol_res$message)
  return(fundeferror_state)
}

#' @export
test_equal.FunDefResultState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  fundef_test_equal_helper(state, incorrect_msg, eq_condition, type = "result")
}

#' @export
test_equal.FunDefOutputState <- function(state, incorrect_msg = NULL) {
  return(fundef_test_equal_helper(state, incorrect_msg, type = "output"))
}

#' @export
test_equal.FunDefErrorState <- function(state, incorrect_msg = NULL) {
  return(fundef_test_equal_helper(state, incorrect_msg, type = "error"))
}

fundef_test_equal_helper <- function(state, incorrect_msg, eq_condition = "equivalent", type = c("result", "output", "error")) {
  type <- match.arg(type)
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$add_details(type = "fundef",
                    case = sprintf("%s_equal", type),
                    eq_condition = eq_condition,
                    student = student_obj,
                    solution = solution_obj,
                    message = incorrect_msg)
  
  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = state$details)
  return(state)
}



