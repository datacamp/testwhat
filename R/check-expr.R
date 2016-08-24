#' Check the result, output or errors thrown by an expression
#' 
#' Run an expression in student and solution environment and compare the result,
#' output or error that is thrown by it.
#' 
#' @param state state to start from (only for \code{check_} functions)
#' @param expr the expression to run
#' @param error_msg custom message in case the expression throws an error while
#'   it shouldn't
#' @param no_error_msg custom message in case the expression doesn't throw an
#'   error while it should
#' @param eq_condition character string indicating how to compare. See
#'   \code{\link{is_equal}}.
#' @param incorrect_msg custom message in case the result, output or error of
#'   the expression does not correspond with the solution
#' @param append Whether or not to append the feedback to feedback built in previous states
#' @param ... S3 stuff
#' 
#' @rdname test_expr
#' @examples
#' \dontrun{
#' # Example 1
#' a <- c(1, 2, 3, 4, 5, 6) 
#' 
#' # SCT option 1 to test if second and fourth element is ok:
#' test_expression_result("a[c(2, 4)]")
#' 
#' # SCT option 2
#' ex() %>% check_expr("a[c(2, 4)]") %>% check_result() %>% check_equal()
#' 
#' # Example 2
#' my_fun <- function() { print('hello') }
#' 
#' # SCT option 1 to test if my_fun() produces correct output:
#' test_expression_output("my_fun()")
#' 
#' # SCT option 2
#' ex() %>% check_expr("my_fun()") %>% check_output() %>% check_equal()
#' }

#' @rdname test_expr
#' @export 
test_expression_result <- function(expr, 
                                   eq_condition = "equivalent",
                                   incorrect_msg = NULL) {
  ex() %>% 
    check_expr(expr) %>% 
    check_result() %>% 
    check_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg, append = is.null(incorrect_msg))
}

#' @rdname test_expr
#' @importFrom utils capture.output
test_expression_output <- function(expr, incorrect_msg = NULL) {
  ex() %>% 
    check_expr(expr) %>% 
    check_output() %>% 
    check_equal(incorrect_msg = incorrect_msg, append = is.null(incorrect_msg))
}

#' @rdname test_expr
#' @export 
test_expression_error <- function(expr, no_error_msg = NULL, incorrect_msg = NULL) {
  ex() %>% 
    check_expr(expr) %>% 
    check_error(no_error_msg = no_error_msg, append = is.null(no_error_msg)) %>% 
    check_equal(incorrect_msg = incorrect_msg, append = is.null(incorrect_msg))
}

#' @rdname test_expr
#' @export
check_expr <- function(state, expr) {
  expr_state <- ExprState$new(state)
  expr_state$set(expr = parse(text = expr))
  return(expr_state)
}

#' @rdname test_expr
#' @export
check_result.ExprState <- function(state, error_msg = NULL, append = TRUE, ...) {
  expr <- state$get("expr")
  run_expr_helper(state, 
                  expr = expr,
                  expr_str = as.character(expr),
                  error_msg = error_msg,
                  append = append,
                  case = "result")
}


#' @rdname test_expr
#' @export
check_output.ExprState <- function(state, error_msg = NULL, append = TRUE, ...) {
  expr <- state$get("expr")
  run_expr_helper(state, 
                  expr = expr,
                  expr_str = as.character(expr),
                  error_msg = error_msg,
                  append = append,
                  case = "output")
}

#' @rdname test_expr
#' @export
check_error.ExprState <- function(state, no_error_msg = NULL, append = TRUE, ...) {
  expr <- state$get("expr")
  run_expr_error_helper(state, 
                        expr = expr,
                        expr_str = as.character(expr),
                        no_error_msg = no_error_msg,
                        append = append)
}

#' @rdname test_expr
#' @export
check_equal.ExprResultState <- function(state, incorrect_msg = NULL, append = TRUE, eq_condition = "equivalent", ...) {
  fundef_check_equal_helper(state, incorrect_msg, eq_condition, append = append, type = "result")
}

#' @rdname test_expr
#' @export
check_equal.ExprOutputState <- function(state, incorrect_msg = NULL, append = TRUE, ...) {
  return(fundef_check_equal_helper(state, incorrect_msg, append = append, type = "output"))
}

#' @rdname test_expr
#' @export
check_equal.ExprErrorState <- function(state, incorrect_msg = NULL, append = TRUE, ...) {
  return(fundef_check_equal_helper(state, incorrect_msg, append = append, type = "error"))
}

run_expr_helper <- function(state, expr, expr_str, error_msg, append, case = c("result", "output")) {
  case <- match.arg(case)
  converter <- switch(case, result = identity, output = capture.output)
  StateType <- switch(case, result = ExprResultState, output = ExprOutputState)
  
  expreval_state <- StateType$new(state)
  expreval_state$add_details(type = "expr",
                             case = sprintf("%s_runs", case),
                             expr_str = expr_str,
                             message = error_msg,
                             append = append,
                             pd = NULL)
  
  sol_res <- tryCatch(converter(eval(expr, envir = state$get("solution_env"))), error = function(e) e)
  if (inherits(sol_res, 'error')) {
    stop(sprintf("Running %s gave an error", expr_str))
  }
  
  stud_res <- tryCatch(converter(eval(expr, envir = state$get("student_env"))), error = function(e) e)
  check_that(is_false(inherits(stud_res, 'error')), feedback = expreval_state$details)
  
  expreval_state$set_details(type = "expr",
                             case = sprintf("%s_correct", case),
                             message = NULL,
                             pd = NULL)
  expreval_state$set(student_object = stud_res, solution_object = sol_res)
  return(expreval_state)
}



run_expr_error_helper <- function(state, expr, expr_str, no_error_msg, append) {

  exprerror_state <- ExprErrorState$new(state)
  exprerror_state$add_details(type = "expr",
                              case = "error_fails",
                              expr_str = expr_str,
                              message = no_error_msg,
                              append = append,
                              pd = NULL)
  
  sol_res <- tryCatch(eval(expr, envir = state$get("solution_env")), error = function(e) e)
  if (!inherits(sol_res, 'error')) {
    stop(sprintf("Running %s didn't give an error, while it should.", expr_str))
  }
  
  stud_res <- tryCatch(eval(expr, envir = state$get("student_env")), error = function(e) e)
  check_that(is_true(inherits(stud_res, 'error')), feedback = exprerror_state$details)
  
  exprerror_state$set_details(type = "expr",
                              case = "error_correct",
                              message = NULL,
                              pd = NULL)
  exprerror_state$set(student_object = stud_res$message, solution_object = sol_res$message)
  return(exprerror_state)
}


fundef_check_equal_helper <- function(state, incorrect_msg, eq_condition = "equivalent", append, type = c("result", "output", "error")) {
  type <- match.arg(type)
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$add_details(type = "expr",
                    case = sprintf("%s_equal", type),
                    eq_condition = eq_condition,
                    student = student_obj,
                    solution = solution_obj,
                    message = incorrect_msg,
                    append = append)
  
  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = state$details)
  return(state)
}

