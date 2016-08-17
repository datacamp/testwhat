#' @export
test_expr <- function(state, expr) {
  expr_state <- ExprState$new(state)
  expr_state$set(expr = parse(text = expr))
  return(expr_state)
}

#' @export
test_result.ExprState <- function(state, error_msg = NULL) {
  expr <- state$get("expr")
  run_expr_helper(state, 
                  expr = expr,
                  expr_str = as.character(expr),
                  error_msg = error_msg,
                  case = "result")
}

#' @export
test_output.ExprState <- function(state, error_msg = NULL) {
  expr <- state$get("expr")
  run_expr_helper(state, 
                  expr = expr,
                  expr_str = as.character(expr),
                  error_msg = error_msg,
                  case = "output")
}

#' @export
test_error.ExprState <- function(state, no_error_msg = NULL) {
  expr <- state$get("expr")
  run_expr_error_helper(state, 
                        expr = expr,
                        expr_str = as.character(expr),
                        no_error_msg = no_error_msg)
}

run_expr_helper <- function(state, expr, expr_str, error_msg = NULL, case = c("result", "output")) {
  case <- match.arg(case)
  converter <- switch(case, result = identity, output = capture.output)
  StateType <- switch(case, result = ExprResultState, output = ExprOutputState)
  
  expreval_state <- StateType$new(state)
  expreval_state$add_details(type = "expr",
                             case = sprintf("%s_runs", case),
                             expr_str = expr_str,
                             message = error_msg,
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

run_expr_error_helper <- function(state, expr, expr_str, no_error_msg = NULL) {

  exprerror_state <- ExprErrorState$new(state)
  exprerror_state$add_details(type = "expr",
                                case = "error_fails",
                                expr_str = expr_str,
                                message = no_error_msg,
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


#' @export
test_equal.ExprResultState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  fundef_test_equal_helper(state, incorrect_msg, eq_condition, type = "result")
}

#' @export
test_equal.ExprOutputState <- function(state, incorrect_msg = NULL) {
  return(fundef_test_equal_helper(state, incorrect_msg, type = "output"))
}

#' @export
test_equal.ExprErrorState <- function(state, incorrect_msg = NULL) {
  return(fundef_test_equal_helper(state, incorrect_msg, type = "error"))
}

fundef_test_equal_helper <- function(state, incorrect_msg, eq_condition = "equivalent", type = c("result", "output", "error")) {
  type <- match.arg(type)
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$add_details(type = "expr",
                    case = sprintf("%s_equal", type),
                    eq_condition = eq_condition,
                    student = student_obj,
                    solution = solution_obj,
                    message = incorrect_msg)
  
  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = state$details)
  return(state)
}
