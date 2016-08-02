test_fun <- function(state, name, index = 1, not_called_msg = NULL) {

  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  student_calls <- find_function_calls(student_pd, name, student_env)
  solution_calls <- find_function_calls(solution_pd, name, solution_env)
  n_student_calls <- length(student_calls)
  n_solution_calls <- length(solution_calls)

  check_sufficient(solution_calls, index, name)
  solution_call <- solution_calls[[index]]

  if (is.null(not_called_msg)) {
    not_called_msg <- build_function_not_called_msg(name, index)
  }
  check_that(is_true(n_student_calls >= index), list(message = not_called_msg))

  fun_state <- FunctionState$new(state)
  fun_state$set(fun_name = name)
  fun_state$set(solution_call = solution_call)
  fun_state$set(student_calls = student_calls)
  return(fun_state)
}

test_arg <- function(state, arg, arg_not_specified_msg = NULL) {
  
  fun_name <- state$get("fun_name")
  solution_call <- state$get("solution_call")
  student_calls <- state$get("student_calls")
  
  if (! arg %in% names(solution_call$args)) {
    stop("The solution call doesn't meet the argument conditions itself.",
         " Make sure that the args you specify in test_function(\"", fun_name, "\", ...)",
         " are actually specified by the corresponding function call in the solution code")
  }

  seq <- seq_along(student_calls)
  res <- numeric()
  for (i in seq) {
    student_call <- student_calls[[i]]

    # Check if the function is called with the right arguments
    if (! arg %in% names(student_call$args)) {
      msg <- as.list(c(message = arg_not_specified_msg,
                       get_line_info(student_call$function_pd)))
    } else {
      res <- c(res, i)
    }
  }

  if(length(res) == 0) {
    # Raise 'mistake', shouldn't go further.
  }


  # update fun_state with remaining options
  state$set(student_calls = student_calls[res])
  
  arg_state <- ArgumentState$new(state)
  arg_state$set(student_object = student)
  arg_state$set(solution_object = solution)
  arg_state$set(student_pd = extract_object_assignment(student_pd, name))
  arg_state$set(solution_pd = extract_object_assignment(solution_pd, name))
  
  return(NULL)
}



# #   # Check if the function is called with the right arguments
# #   args_specified <- has_arguments(student_call$call, args, ignore, allow_extra)
# #   if (!args_specified) {
# #     if (is.null(args_specified_feedback)) {
# #       if (is.null(args_not_specified_msg)) {
# #         args_not_specified_msg <- build_function_args_not_specified_msg(name, args, n_args, allow_extra)
# #       }
# #       args_specified_feedback <- as.list(c(message = args_not_specified_msg,
# #                                            get_line_info(student_call$function_pd)))
# #     }
# #     next
# #   } else {
# #     args_specified_passed <- TRUE
# #   }
# #   
# #   # Test if the specified arguments are correctly called
# #   solution_args <- extract_arguments(solution_call$call, args, eval, env = solution_env)
# #   if (any(sapply(solution_args, function(x) isTRUE(try(all.equal(x, tryerrorstring), silent = TRUE))))) {
# #     stop(sprintf("There are arguments in the %s function call of %s() that cause errors when evaluated.", get_num(index), name))
# #   }
# #   
# #   for ()
# # }
# 
# check_state <- function(state, ...) {
#   if(!c(...) %in% names(state)) {
#     stop("state doesn't contain all required elements.")
#   }
# }
