#' Test if student specified argument
#' 
#' @param arg name of argument to specify
#' @param arg_not_specified_msg custom message in case argument was not specified
#' 
#' @export
test_arg <- function(state, arg, arg_not_specified_msg = NULL) {
  
  solution_call <- state$get("solution_call")
  student_calls <- state$get("student_calls")
  
  arg_state <- ArgumentState$new(state)
  arg_state$add_details(type = "argument",
                        case = "specified",
                        name = arg,
                        message = arg_not_specified_msg)
  
  if (! arg %in% names(solution_call$args)) {
    stop(" Make sure that the arguments you specify are actually specified", 
         " by the corresponding function call in the solution code.")
  }
  
  res <- logical(length(student_calls))
  details <- NULL
  for (i in seq_along(student_calls)) {
    student_call <- student_calls[[i]]
    if (isTRUE(is.na(student_call))) next
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      arg_state$set_details(pd = student_call$pd)
      details <- arg_state$details
    }
    
    # Check if the function is called with the right arguments
    if (arg %in% names(student_call$args)) {
      arg_state$log(index = i, arg = arg, success = TRUE)
      res[i] <- TRUE
    } else {
      arg_state$log(index = i, arg = arg, success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- arg_state$details
  }
  check_that(is_gte(sum(res), 1), feedback = details)
  
  student_args <- student_calls
  student_args[res] <- lapply(student_args[res], function(x) x$args[[arg]])
  student_args[!res] <- NA
  arg_state$set(student_args = student_args)
  arg_state$set(solution_arg = solution_call$args[[arg]])
  
  arg_state$set_details(case = "correct",
                        message = NULL)
  
  return(arg_state)
}

