#' @export
test_fun <- function(state, name, index = 1, not_called_msg = NULL) {
  test_call(state, name = name, index = index, not_called_msg = not_called_msg, type = "function")
}

#' @export
test_op <- function(state, name, index = 1, not_called_msg = NULL) {
  test_call(state, name = name, index = index, not_called_msg = not_called_msg, type = "operator")
}

test_call <- function(state, name, index, not_called_msg, type = c("function", "operator")) {
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
  
  student_calls[-options] <- NULL
  call_state$set(solution_call = solution_call)
  call_state$set(student_calls = student_calls)
  return(call_state)
}

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
  
  res <- numeric()
  details <- NULL
  for (i in seq_along(student_calls)) {
    student_call <- student_calls[[i]]
    if (is.null(student_call)) next
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      arg_state$set_details(pd = student_call$pd)
      details <- arg_state$details
    }
    
    # Check if the function is called with the right arguments
    if (arg %in% names(student_call$args)) {
      arg_state$log(index = i, arg = arg, success = TRUE)
      res <- c(res, i)
    } else {
      arg_state$log(index = i, arg = arg, success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- arg_state$details
  }
  check_that(is_gte(length(res), 1), feedback = details)
  
  student_args <- student_calls
  student_args[res] <- lapply(student_args[res], function(x) x$args[[arg]])
  student_args[-res] <- NULL
  arg_state$set(student_args = student_args)
  arg_state$set(solution_arg = solution_call$args[[arg]])
  
  arg_state$set_details(case = "correct",
                        message = NULL)
  
  return(arg_state)
}

#' @export
test_equal.ArgumentState <- function(state, incorrect_msg = NULL, eval = TRUE, eq_condition = "equivalent") {
  
  solution_arg <- state$get("solution_arg")
  student_args <- state$get("student_args")
  
  state$add_details(type = "argument",
                    case = "equal",
                    eval = eval,
                    eq_condition = eq_condition,
                    message = incorrect_msg)
  
  # Test if the specified arguments are correctly called
  solution_obj <- eval_argument(solution_arg,
                                eval = eval, 
                                env = state$get("solution_env"))
  
  if (isTRUE(try(all.equal(solution_obj, tryerrorstring), silent = TRUE))) {
    stop("test_equal() found an argument that causes an error when evaluated.")
  }

  res <- numeric()
  details <- NULL
  for (i in seq_along(student_args)) {
    student_arg <- student_args[[i]]
    if (is.null(student_arg)) next
    student_obj <- eval_argument(student_arg,
                                 eval = eval,
                                 env = state$get("student_env"))
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      if (is_dots(student_arg)) {
        pd <- state$student_calls[[i]]$pd
        is_dots <- TRUE
      } else {
        pd <- student_arg$pd
        is_dots <- FALSE
      }
      state$set_details(student = student_obj,
                        solution = solution_obj,
                        pd = pd,
                        is_dots = is_dots)
      details <- state$details
    }
    
    # Check if the function arguments correspond
    if (is_equal(student_obj, solution_obj, eq_condition)) {
      state$log(index = i, success = TRUE)
      res <- c(res, i)
    } else {
      state$log(index = i, success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- state$details
  }
  
  check_that(is_gte(length(res), 1), feedback = details)
  
  return(state)
}

tryerrorstring <- "try-error-in-evaluation"

eval_argument <- function(arg, eval, env) {
  if (is_dots(arg)) {
    return(lapply(arg, eval_argument, eval, env))
  }
  if (eval) {
    obj <- try(eval(arg$expr, envir = env), silent = TRUE)
    if (inherits(obj, "try-error")) {
      obj <- tryerrorstring
    }
  } else {
    obj <- stringify(arg$expr)
  }
  return(obj)
}

is_dots <- function(arg) {
  is.list(arg) && ! "expr" %in% names(arg)
}

stringify <- function(arg) {
  x <- deparse(arg)
  x <- gsub("\\s|;", "", x)
  x <- gsub("'", "\"", x)
  x <- gsub("FALSE", "F", x)
  x <- gsub("TRUE", "T", x)
  return(x)
}
