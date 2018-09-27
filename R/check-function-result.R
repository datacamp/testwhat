#' Check the result of a function call/operation
#'
#' @param error_msg feedback message in case the student function call at the
#'   mentioned index generated an error.
#' @param incorrect_msg  feedback message in case the evaluation was not the
#'   same as in the solution.
#' @param eq_condition character string indicating how to compare. See
#'   \code{\link{is_equal}}.
#' @param eq_fun optional argument to specify a custom equality function. The
#'   function should take two arguments and always return a single boolean
#'   value: \code{TRUE} or \code{FALSE}.
#' @param state the state to start from (for \code{check_} functions)
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states
#' @param ... S3 stuff
#'
#' @examples
#' \dontrun{
#' # Example 1
#' mean(1:3)
#'
#' # SCT
#' ex() %>% check_function("mean") %>% check_result() %>% check_equal()
#' }
#' @name check_function_result

#' @rdname check_function_result
#' @export
check_result.OperationState <- function(state, error_msg = NULL, append = TRUE, ...) {
  check_call_result(state, error_msg = error_msg, append = append, type = "operator")
}

#' @rdname check_function_result
#' @export
check_result.FunctionState <- function(state, error_msg = NULL, append = TRUE, ...) {
  check_call_result(state, error_msg = error_msg, append = append, type = "function")
}

#' @rdname check_function_result
#' @export
check_equal.FunctionResultState <- function(state, eq_condition = "equivalent", eq_fun = NULL, incorrect_msg = NULL, append = TRUE, ...) {
  check_call_result_equal(state, eq_condition = eq_condition, eq_fun = eq_fun, incorrect_msg = incorrect_msg, append = append, type = "function")
}

#' @rdname check_function_result
#' @export
check_equal.OperationResultState <- function(state, eq_condition = "equivalent", eq_fun = NULL, incorrect_msg = NULL, append = TRUE, ...) {
  check_call_result_equal(state, eq_condition = eq_condition, eq_fun = eq_fun, incorrect_msg = incorrect_msg, append = append, type = "operator")
}

check_call_result <- function(state, error_msg, append, type = c("function", "operator")) {
  type <- match.arg(type)
  CallResultState <- switch(type, `function` = FunctionResultState, operator = OperationResultState)
  
  solution_call <- state$get("solution_call")
  student_call <- state$get("student_call")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  callresult_state <- CallResultState$new(state)
  callresult_state$add_details(type = type,
                               case = "result_runs",
                               message = error_msg,
                               pd = student_call$pd,
                               append = append)
  
  sol_res <- tryCatch(base::eval(solution_call$call, envir = solution_env), error = function(e) e)
  if (inherits(sol_res, 'error')) {
    stop(sprintf("Running %s gave an error: %s", deparse(solution_call$call), sol_res$message))
  }

  stud_res <- tryCatch(base::eval(student_call$call, envir = student_env), error = function(e) e)
  
  check_that(is_false(inherits(stud_res, 'error')), feedback = callresult_state$details)
  
  callresult_state$set(student_call_result = stud_res,
                       solution_call_result = sol_res)
  callresult_state$set_details(case = "result_correct",
                               message = NULL)
  return(callresult_state)
}



check_call_result_equal <- function(state, eq_condition, eq_fun, incorrect_msg, append, type = c("function", "operator")) {
  type <- match.arg(type)
  sol_res <- state$get("solution_call_result")
  stud_res <- state$get("student_call_result")
  state$add_details(type = type,
                    case = "result_equal",
                    eq_condition = eq_condition,
                    message = incorrect_msg,
                    student = stud_res,
                    solution = sol_res,
                    append = append)
  
  if (is.null(eq_fun)) {
    eq_fun <- function(x, y) is_equal(x, y, eq_condition)
  }

  check_that(expect_true(eq_fun(stud_res, sol_res)), feedback = state$details)
  
  return(state)
}

# Deprecated

test_function_result <- function(name = NULL,
                                 index = 1,
                                 eq_condition = "equivalent",
                                 not_called_msg = NULL,
                                 error_msg = NULL,
                                 incorrect_msg = NULL) {
  fail_if_v2_only()
  ex() %>% 
    check_function(name, index = index, not_called_msg = not_called_msg, append = is.null(not_called_msg)) %>% 
    check_result(error_msg = error_msg, append = is.null(error_msg)) %>%
    check_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg, append = is.null(incorrect_msg))
}


