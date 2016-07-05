#' Test whether a student correctly called a function 
#'
#' Test whether a student called a function, possibly with certain arguments, 
#' correctly. Note that \code{test_function} and \code{test_function_v2} are now
#' identical and either can be used, although the latter is likely to be phased
#' out in the future.
#' 
#' @param name  name of the function to test.
#' @param args  character vector of argument names that the student should have
#' supplied in the function calls.
#' @param index  integer that specifies which call of \code{name} in the solution 
#' code will be checked.
#' @param ignore character vector of argument names that should not be tested
#' (useful in combination with \code{allow_extra = FALSE} to allow certain
#' arguments to be ignored, but not others).
#' @param allow_extra  indicates whether extra arguments not specified by
#' \code{args} or \code{ignore} are allowed in the student's function calls.
#' @param eval  logical vector indicating whether the corresponding argument
#' should be evaluated before testing. Setting this to \code{FALSE} can be
#' useful, e.g., to test whether the student supplied a large predefined
#' object, as only the corresponding \code{\link{name}} is compared in this
#' case (use with care!).
#' @param eq_condition  character vector indicating how to perform the
#' comparison for each argument. See \code{\link{test_object}}
#' @param not_called_msg custom feedback message in case the student did not call the
#' function often enough.
#' @param args_not_specified_msg custom feedback message in case the student did call the function
#' with the arguments listed in \code{args}
#' @param incorrect_msg custom feedback message in case the student did not call the
#' function with the same argument values as in the sample solution. 
#' You can specify a vector of arguments with the same length as \code{args}, to have
#' argument-specific custom feedback.
#' 
#' @examples
#' \dontrun{
#' # Suppose the solution contains: mean(1:3, na.rm = TRUE)
#' # To test this submission, provide the following in the sct
#' test_function("mean", c("x", "na.rm"))
#' }
#'
#' @export
test_function <- function(name, 
                          args = NULL, 
                          index = 1,
                          ignore = NULL,
                          allow_extra = TRUE,
                          eval = TRUE,
                          eq_condition = "equivalent",
                          not_called_msg = NULL, 
                          args_not_specified_msg = NULL,
                          incorrect_msg = NULL) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  init_tags(fun = "test_function")

  n_args <- length(args)
  if (!is.null(incorrect_msg) && length(incorrect_msg) < n_args) {
    incorrect_msg <- rep(incorrect_msg[1], n_args)
  }
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  
  # Find all function calls in the student and solution code
  student_calls <- find_function_calls(student_pd, name, student_env)
  solution_calls <- find_function_calls(solution_pd, name, solution_env)
  n_student_calls <- length(student_calls)
  n_solution_calls <- length(solution_calls)

  check_sufficient(solution_calls, index, name)
  solution_call <- solution_calls[[index]]
  
  if (n_args > 0 && !has_arguments(solution_call$call, args, ignore, allow_extra)) {
      stop("The solution call doesn't meet the argument conditions itself.",
           " Make sure that the args you specify in test_function(\"", name, "\", ...)", name, "()",
           " are actually specified by the corresponding function call in the solution code")
  }
  
  if (is.null(not_called_msg)) {
    not_called_msg <- build_function_not_called_msg(name, index)
  }
  test_what(expect_true(n_student_calls >= index), list(message = not_called_msg))
  
  if (n_args > 0) {
    args_specified_passed <- FALSE
    args_correct_passed <- FALSE
    args_specified_feedback <- NULL
    args_correct_feedback <- NULL
    
    seq <- get_seq(name, stud_indices = 1:n_student_calls, sol_index = index)
    for (i in seq) {
      student_call <- student_calls[[i]]
      
      # Check if the function is called with the right arguments
      args_specified <- has_arguments(student_call$call, args, ignore, allow_extra)
      if (!args_specified) {
        if (is.null(args_specified_feedback)) {
          if (is.null(args_not_specified_msg)) {
            args_not_specified_msg <- build_function_args_not_specified_msg(name, args, n_args, allow_extra)
          }
          args_specified_feedback <- list(message = args_not_specified_msg,
                                          line_start = student_call$line1,
                                          line_end = student_call$line2,
                                          column_start = student_call$col1,
                                          column_end = student_call$col2)  
        }
        next
      } else {
        args_specified_passed <- TRUE
      }
      
      # Test if the specified arguments are correctly called
      solution_args <- extract_arguments(solution_call$call, args, eval, env = solution_env)
      student_args <- extract_arguments(student_call$call, args, eval, env = student_env)
      
      args_correct_vec <- mapply(is_equal, student_args, solution_args, eq_condition)
      if (!all(args_correct_vec)) {
        score <- sum(args_correct_vec)
        if (is.null(args_correct_feedback) || args_correct_feedback$score < score) {
          if (is.null(incorrect_msg)) {
            incorrect_args <- args[!args_correct_vec]
            feedback_msg <- build_function_incorrect_msg(name, incorrect_args)
          } else {
            feedback_msg <- incorrect_msg[!args_correct_vec][1]
          }
          args_correct_feedback <- list(message = feedback_msg,
                                        line_start = student_call$line1,
                                        line_end = student_call$line2,
                                        column_start = student_call$col1,
                                        column_end = student_call$col2,
                                        score = score)
        }
        next
      } else {
        args_correct_passed <- TRUE
        # We have a winner.
        set_used(name, stud_index = i, sol_index = index)
        break
      }
    }
    
    if (!args_correct_passed) {
      # Still need something that fails...
      if (!args_specified_passed) {
        test_what(fail(), args_specified_feedback)
      } else {
        test_what(fail(), args_correct_feedback)
      }
    }
  }
}

#' @rdname test_function
#' @export
test_function_v2 <- test_function
  
has_arguments <- function(call, args, ignore = NULL, allow_extra = TRUE) {
  if (allow_extra) 
    all(args %in% names(call)[-1])
  else {
    supplied <- setdiff(names(call)[-1], ignore)
    compare(args, supplied)$equal
  }
}

# Extract specified arguments from a function call and evaluate if necessary
extract_arguments <- function(call, args, eval = TRUE, env) {
  mapply(function(arg, eval) {
    object <- call[[arg]]
    if (eval && (is.name(object) || is.call(object) || is.expression(object))) {
      object <- try(eval(object, envir = env), silent = TRUE)
      if (inherits(object, "try-error")) {
        object <- "try-error"
      }
    }
    object
  }, args, eval, SIMPLIFY = FALSE)
}

