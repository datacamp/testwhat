test_fun <- function(state, name, index = 1, not_called_msg = NULL) {

  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  fun_state <- FunctionState$new(state)
  fun_state$add_details(type = "function",
                        case = "called",
                        name = name,
                        index = index)
  
  student_calls <- find_function_calls(student_pd, name, student_env)
  solution_calls <- find_function_calls(solution_pd, name, solution_env)
  n_student_calls <- length(student_calls)
  n_solution_calls <- length(solution_calls)

  check_sufficient(solution_calls, index, name)
  solution_call <- solution_calls[[index]]

  check_that(is_true(n_student_calls >= index), 
             feedback = list(message = not_called_msg,
                             details = fun_state$get("details"),
                             pd = NULL))
  
  state$update_blacklist()
  state$set(active_name = name)
  state$set(active_sol_index = index)
  options <- state$get_options(length(student_calls))
  
  student_calls[-options] <- NULL
  fun_state$set(solution_call = solution_call)
  fun_state$set(student_calls = student_calls)
  return(fun_state)
}

test_arg <- function(state, arg, arg_not_specified_msg = NULL) {
  
  solution_call <- state$get("solution_call")
  student_calls <- state$get("student_calls")
  
  state$set_details(case = "correct")
  arg_state <- ArgumentState$new(state)
  arg_state$add_details(type = "argument",
                        case = "specified",
                        name = arg)
  
  if (! arg %in% names(solution_call$args)) {
    stop(" Make sure that the arguments you specify are actually specified", 
         " by the corresponding function call in the solution code.")
  }

  res <- numeric()
  feedback <- NULL
  for (i in seq_along(student_calls)) {
    student_call <- student_calls[[i]]
    if (is.null(student_call)) next
    if (is.null(feedback)) {
      feedback <- list(message = arg_not_specified_msg,
                       details = arg_state$get("details"),
                       pd = student_call$function_pd)  
    }
    
    # Check if the function is called with the right arguments
    if (arg %in% names(student_call$args)) {
      arg_state$log(index = i, arg = arg, success = TRUE)
      res <- c(res, i)
    } else {
      arg_state$log(index = i, arg = arg, success = FALSE)
    }
  }

  check_that(is_gte(length(res), 1), feedback = feedback)
  
  student_args <- student_calls
  student_args[res] <- lapply(student_args[res], function(x) x$args[[arg]])
  student_args[-res] <- NULL
  arg_state$set(student_args = student_args)
  arg_state$set(solution_arg = solution_call$args[[arg]])
  return(arg_state)
}


test_equal.ArgumentState <- function(state, incorrect_msg = NULL, eval = TRUE, eq_condition = "equivalent") {
  
  solution_arg <- state$get("solution_arg")
  student_args <- state$get("student_args")
  
  state$set_details(case = "equal",
                    eval = eval,
                    eq_condition = eq_condition)
  
  # Test if the specified arguments are correctly called
  
  solution_obj <- eval_argument(solution_arg$expr, 
                                eval = eval, 
                                env = state$get("solution_env"))
  if (isTRUE(try(all.equal(solution_obj, tryerrorstring), silent = TRUE))) {
    stop("test_equal() found an argument that causes an error when evaluated.")
  }
  
  seq <- seq_along(student_args)
  res <- numeric()
  feedback <- NULL
  for (i in seq) {
    student_arg <- student_args[[i]]
    if (is.null(student_arg)) next
    student_obj <- eval_argument(student_arg$expr,
                                 eval = eval,
                                 env = state$get("student_env"))
    if (is.null(feedback)) {
      state$set_details(student = student_obj,
                        solution = solution_obj)
      feedback <- list(message = incorrect_msg,
                       details = state$get("details"),
                       pd = student_arg$pd)
    }
    
    # Check if the function arguments correspond
    if (is_equal(student_obj, solution_obj, eq_condition)) {
      state$log(index = i, success = TRUE)
      res <- c(res, i)
    } else {
      state$log(index = i, success = FALSE)
    }
  }
  
  check_that(is_gte(length(res), 1), feedback = feedback)
  
  return(state)
}

tryerrorstring <- "try-error-in-test-function"

eval_argument <- function(arg, eval, env) {
  if (eval) {
    obj <- try(eval(arg, envir = env), silent = TRUE)
    if (inherits(obj, "try-error")) {
      obj <- tryerrorstring
    }
  } else {
    obj <- stringify(arg)
  }
  return(obj)
}

stringify <- function(arg) {
  x <- deparse(arg)
  x <- gsub("\\s|;", "", x)
  x <- gsub("'", "\"", x)
  x <- gsub("FALSE", "F", x)
  x <- gsub("TRUE", "T", x)
  return(x)
}

is.callable <- function(x) {
  is.name(x) || is.call(x) || is.expression(x)
}

