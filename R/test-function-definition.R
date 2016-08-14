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
test_fun_def <- function(state, name, undefined_msg = NULL) {
  fundef_state <- test_defined(state, name, undefined_msg, type = "fundef")
  fundef_state$set(name = name)
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
test_result <- function(state, ...) {
  # build fun call with ...
  sol_res <- tryCatch(do.call(state$get("name"), list(...), envir = state$get("solution_env")),
                      error = function(e) e)
  if (inherits(sol_res, 'error')) {
    stop("Running ... gave an error")
  }
  
  stud_res <- tryCatch(do.call(state$get("name"), list(...), envir = state$get("student_env")),
                       error = function(e) e)
  
  check_that(is_false(inherits(stud_res, 'error')),
             feedback = state$details)
  
  # call test_equal
  return(test)
}

build_arg_str <- function(...) {
  arg_list <- list(...)
  nms <- names(arg_list)
  if (is.null(names)) {
    nms <- rep("", length(arg_lst))
  }
  arg_str <- "("
  for (i in seq_along(arg_list)) {
    if (nms[i] == "") {
      arg_str %+=0% arg_list[[i]]
    } else {
      arg_str %+=0% sprintf("%s = %s", nms[i], arg_list[[i]])
    }
  }
  arg_str <- 
  if (is.null(nms)) {
    # no names at all
    argstr <- paste
  }
}

#' @export
test_output <- function(state, ...) {
  # build fun call with ...
  
  # do the call
  
  # call test_equal
}

#' @export
test_error <- function(state, ...) {
  # build fun call with arguments
  
  # do the call
  
  # call test_equal
}







