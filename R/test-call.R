#' Test if user called function/operator
#' 
#' @param state state to start from
#' @param name name of function/function as a string, e.g. \code{"mean"} or \code{"+"}
#' @param index which call of \code{name} in the solution to check.
#' @param not_called_msg custom message in case \code{name} was not called sufficiently
#' @name test_call


#' @export
#' @rdname test_call
test_fun <- function(state, name, index = 1, not_called_msg = NULL) {
  test_call_helper(state, name = name, index = index, not_called_msg = not_called_msg, type = "function")
}

#' @export
#' @rdname test_call
test_op <- function(state, name, index = 1, not_called_msg = NULL) {
  test_call_helper(state, name = name, index = index, not_called_msg = not_called_msg, type = "operator")
}

test_call_helper <- function(state, name, index, not_called_msg, type = c("function", "operator")) {
  type <- match.arg(type)
  finder <- switch(type, `function` = find_function_calls, operator = find_operators)
  CallState <- switch(type, `function` = FunctionState, operator = OperationState)
  
  call_state <- CallState$new(state)
  call_state$add_details(type = type,
                         case = "called",
                         name = name,
                         index = index,
                         message = not_called_msg,
                         pd = NULL)
  
  student_calls <- finder(pd = state$get("student_pd"), 
                          name = name, 
                          env = state$get("student_env"))
  solution_calls <- finder(pd = state$get("solution_pd"),
                           name = name,
                           env = state$get("solution_env"))
  
  check_sufficient(solution_calls, index, name)
  solution_call <- solution_calls[[index]]
  
  check_that(is_true(length(student_calls) >= index), feedback = call_state$details)
  
  # update the case for future tests
  call_state$set_details(case = "correct",
                         message = NULL)
  
  # manage blacklisting of operators
  state$update_blacklist()
  state$set(active_name = name)
  state$set(active_sol_index = index)
  options <- state$get_options(length(student_calls))
  
  student_calls[-options] <- NA
  call_state$set(solution_call = solution_call)
  call_state$set(student_calls = student_calls)
  return(call_state)
}

