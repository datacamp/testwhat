#' @export
test_result.OperationState <- function(state, error_msg = NULL) {
  test_call_result(state, error_msg = error_msg, type = "operator")
}

#' @export
test_result.FunctionState <- function(state, error_msg = NULL) {
  test_call_result(state, error_msg = error_msg, type = "function")
}

test_call_result <- function(state, error_msg, type = c("function", "operator")) {
  type <- match.arg(type)
  CallResultState <- switch(type, `function` = FunctionResultState, operator = OperationResultState)
  
  solution_call <- state$get("solution_call")
  student_calls <- state$get("student_calls")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  callresult_state <- CallResultState$new(state)
  callresult_state$add_details(type = type,
                               case = "result_runs",
                               message = error_msg)
  
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


#' @export
test_equal.FunctionResultState <- function(state, eq_condition = "equivalent", incorrect_msg = NULL) {
  test_call_result_equal(state, eq_condition = eq_condition, incorrect_msg = incorrect_msg, type = "function")
}

#' @export
test_equal.OperationResultState <- function(state, eq_condition = "equivalent", incorrect_msg = NULL) {
  test_call_result_equal(state, eq_condition = eq_condition, incorrect_msg = incorrect_msg, type = "operator")
}

test_call_result_equal <- function(state, eq_condition, incorrect_msg, type = c("function", "operator")) {
  type <- match.arg(type)
  solution_call <- state$get("solution_call")
  student_calls <- state$get("student_calls")
  state$add_details(type = type,
                    case = "result_equal",
                    eq_condition = eq_condition,
                    message = incorrect_msg)
  
  sol_res <- solution_call$result
  
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
    if (is_equal(stud_res, sol_res, eq_condition)) {
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


