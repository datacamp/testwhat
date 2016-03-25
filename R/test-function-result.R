#' Check the result of a function call
#'
#' TODO More information here.
#' 
#' @param name name of the function whose output you would like to check.
#' @param index Ordinal number of the call you want to check (both student and solution!).
#' @param not_called_msg feedback message in case the function is not retrieved.
#' @parem eval_error_msg feedback message in case the student function call at the mentioned index generated an error.
#' @param incorrect_msg  feedback message in case the evaluation was not the same as in the solution.
#' @inheritParams test_function
#'
#' @export
test_function_result <- function(name = NULL,
                                index = 1,
                                eq_condition = "equivalent",
                                not_called_msg = NULL,
                                eval_error_msg = NULL,
                                incorrect_msg = NULL) {
  
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  init_tags(fun = "test_subexpr_result")
  
  # Find all function calls in the student and solution code
  student_calls <- find_function_calls(student_pd, name, student_env)
  solution_calls <- find_function_calls(solution_pd, name, solution_env)
  n_student_calls <- length(student_calls)
  n_solution_calls <- length(solution_calls)
  
  # Check if index exists in solution
  if(index > length(solution_calls)) {
    stop(sprintf("There aren't %s calls of `%s()` available in the solution.", index, name))
  }
  solution_call <- solution_calls[[index]]
  
  if(is.null(not_called_msg)) {
    not_called_msg = sprintf("The system wants to check the %s call of `%s()`, but couldn't find it.", get_num(index), name)
  }
  test_what(expect_true(n_student_calls >= index), feedback = list(message = not_called_msg))
  student_call <- student_calls[[index]]
  
  if(is.null(eval_error_msg)) {
    eval_error_msg <- sprintf("Evaluating the %s call of `%s` generated an error.", get_num(index), name)
  }
  solution_result <- try(eval(solution_call$call), silent = TRUE)
  if(inherits(solution_result, "try-error")) stop(paste("SOLUTION:", eval_error_msg))
  student_result <- try(eval(student_call$call), silent = TRUE)
  test_what(expect_false(inherits(student_result, "try-error")),  feedback = list(message = eval_error_msg,
                                                                                  line_start = student_call$line1,
                                                                                  line_end = student_call$line2,
                                                                                  column_start = student_call$col1,
                                                                                  column_end = student_call$col2))
  
  # Order the columns alphabetically
  if(!is.null(attr(solution_result, "names")) && !is.null(attr(student_result, "names"))) {
    solution_result <- solution_result[sort(names(solution_result))]
    student_result <- student_result[sort(names(student_result))]
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("The output of the %s call of `%s` isn't what it should be. Try again.", get_num(index), name)
  }
  test_what(expect_true(is_equal(solution_result, student_result, eq_condition)),
            feedback = list(message = incorrect_msg,
                            line_start = student_call$line1,
                            line_end = student_call$line2,
                            column_start = student_call$col1,
                            column_end = student_call$col2))
}
