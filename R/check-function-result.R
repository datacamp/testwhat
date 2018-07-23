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
  student_calls <- state$get("student_calls")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  callresult_state <- CallResultState$new(state)
  callresult_state$add_details(type = type,
                               case = "result_runs",
                               message = error_msg,
                               append = append)
  
  sol_res <- tryCatch(base::eval(solution_call$call, envir = solution_env), error = function(e) e)
  if (inherits(sol_res, 'error')) {
    stop(sprintf("Running %s gave an error: %s", deparse(solution_call$call), sol_res$message))
  }
  solution_call$result <- sol_res
  
  res <- logical(length(student_calls))
  details <- NULL
  for (i in seq_along(student_calls)) {
    student_call <- student_calls[[i]]
    if (isTRUE(is.na(student_call))) next
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      callresult_state$set_details(pd = student_call$pd)
      details <- callresult_state$details
    }
    
    stud_res <- tryCatch(base::eval(student_call$call, envir = student_env), error = function(e) e)
    
    # Check if the call passed
    if (!inherits(stud_res, 'error')) {
      callresult_state$log(index = i, arg = 'none', success = TRUE)
      student_calls[[i]]$result <- stud_res
      res[i] <- TRUE
    } else {
      callresult_state$log(index = i, arg = 'none', success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- callresult_state$details
  }
  check_that(is_gte(sum(res), 1), feedback = details)
  
  student_calls[!res] <- NA
  callresult_state$set(student_calls = student_calls,
                       solution_call = solution_call)
  
  callresult_state$set_details(case = "result_correct",
                               message = NULL)
  return(callresult_state)
}



check_call_result_equal <- function(state, eq_condition, eq_fun, incorrect_msg, append, type = c("function", "operator")) {
  type <- match.arg(type)
  solution_call <- state$get("solution_call")
  student_calls <- state$get("student_calls")
  state$add_details(type = type,
                    case = "result_equal",
                    eq_condition = eq_condition,
                    message = incorrect_msg,
                    append = append)
  
  sol_res <- solution_call$result
  
  if (is.null(eq_fun)) {
    eq_fun <- function(x, y) is_equal(x, y, eq_condition)
  }

  res <- logical(length(student_calls))
  details <- NULL
  for (i in seq_along(student_calls)) {
    student_call <- student_calls[[i]]
    if (isTRUE(is.na(student_call))) next
    stud_res <- student_call$result
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      state$set_details(student = stud_res,
                        solution = sol_res,
                        pd = student_call$pd)
      details <- state$details
    }
    
    # Check if the function arguments correspond
    if (eq_fun(stud_res, sol_res)) {
      state$log(index = i, success = TRUE)
      res[i] <- TRUE
    } else {
      state$log(index = i, success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- state$details
  }
  
  check_that(is_gte(sum(res), 1), feedback = details)
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


