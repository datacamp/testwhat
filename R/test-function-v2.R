#' Test whether a student correctly called a function (V2)
#'
#' Test whether a student called a function with certain arguments as least as
#' many times as in a sample solution.  If yes, test for each function call in
#' the sample solution whether the student called the function with the same
#' argument values.
#'
#' This test is implemented using \code{\link{test_that}}.  When testing
#' whether argument values are the same, small numeric differences or
#' differences in attributes are allowed.
#'
#' See vignette \code{"testwhat-intro"} for examples.
#'
#' @param name  name of the function to test.
#' @param args  character vector of argument names that the student should have
#' supplied in the function calls.  If no argument names are given, it is only
#' tested whether the student called the function at least as many times as in
#' the sample solution.
#' @param ignore character vector of argument names that should not be tested
#' (useful in combination with \code{allow_extra = FALSE} to allow certain
#' arguments to be ignored, but not others).
#' @param allow_extra  indicates whether extra arguments not specified by
#' \code{args} or \code{ignore} are allowed in the student's function calls.
#' @param eval  logical vector indicating whether the corresponding argument
#' should be evaluated before testing.  Setting this to \code{FALSE} can be
#' useful, e.g., to test whether the student supplied a large predefined
#' object, as only the corresponding \code{\link{name}} is compared in this
#' case (use with care!).
#' @param index  Which command to check on (both in solution and student).
#' If index is specified, it is checked whether solution and student contain
#' the same amount of commands. index = NULL by default, i.e. the entire submission is checked.
#' @param eq_condition  character vector indicating how to perform the
#' comparison for each argument.  Possible values are \code{"equivalent"}
#' (the default), \code{"equal"} and \code{"identical"}.  See
#' \code{\link{is_equivalent_to}}, \code{\link{equals}}, and
#' \code{\link{is_identical_to}}, respectively.
#' @param student_code  character string containing the student's code.
#' @param solution_code  character string containing the sample solution code.
#' @param not_called_msg  feedback message in case the student did not call the
#' function at least as often as in the solution code.
#' @param incorrect_msg  feedback message in case the student did not call the
#' function with the same argument values as in the sample solution.  If
#' there are multiple function calls in the sample solution, a vector of
#' feedback messages can be supplied.
#' @param incorrect_number_of_calls_msg feedback message in case the number of commands
#' in the solution does not correspond to the solution. (only used if index is not NULL.)
#' @inheritParams test_object
#'
#' @examples
#' \dontrun{
#' # Suppose the solution contains: mean(1:3, na.rm = TRUE)
#' # To test this submission, provide the following in the sct
#' test_function("mean", c("x", "na.rm"))
#' }
#'
#' # Other examples: see SCT design guide
#'
#' @export
test_function_v2 <- function(name, args = NULL, ignore = NULL,
                             allow_extra = TRUE,
                             eval = TRUE,
                             index = NULL,
                             eq_condition = "equivalent",
                             student_code = get_student_code(),
                             solution_code = get_solution_code(),
                             student_env = .GlobalEnv,
                             solution_env = get_solution_env(),
                             not_called_msg = NULL, incorrect_msg = NULL,
                             incorrect_number_of_calls_msg = NULL,
                             index_not_called_msg = NULL) {
  
  n_args <- length(args)
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  
  arg_text <- build_arg_text(n_args, args)
  
  # remove the pipe operator from the calls, if present.
  student_code_parts = get_clean_lines(code = student_code)
  solution_code_parts = get_clean_lines(code = solution_code)
  
  # paste together again, all code is considered
  student_code <- paste0(student_code_parts, separator = "\n", collapse = "")
  solution_code <- paste0(solution_code_parts, separator = "\n", collapse = "")
  additionaltext <- ""
  
  # Find all function calls in the student and solution code
  student_calls <- find_function_calls(name, student_code, student_env)
  solution_calls <- find_function_calls(name, solution_code, solution_env)
  
  if(!is.null(index)) {
    if (is.null(index_not_called_msg)) {
      index_not_called_msg <- build_not_enough_calls_text(name, index)
    }
    # Check if index exists
    index_ok <- test_sufficient_length(student_calls, index, index_not_called_msg)
    if(isTRUE(index_ok)) {
      test_that(sprintf("Function %s()%s is correctly called at index %i", name, arg_text, index), {
        functioncalltext <- build_function_call_text(index)
        # The student call we need is at index for student and solution
        student_calls <- student_calls[index]
        solution_calls <- solution_calls[index]
        
        if (n_args > 0) {
          has_args <- isTRUE(have_arguments(student_calls, args, ignore, allow_extra))
          if (is.null(not_called_msg)) {
            not_called_msg <- build_not_called_msg(n_solution_calls = 1, name, arg_text, functioncalltext)
          }
          # Check if the function is called with the right arguments
          args_ok <- expect_that(has_args, is_true(), failure_msg = not_called_msg)
        }
        student_args <- extract_arguments(student_calls[[1]], args, eval, env = student_env)
        solution_args <- extract_arguments(solution_calls[[1]], args, eval, env = solution_env)
        correct <- mapply(is_equal, student_args, solution_args, eq_condition)
        correct <- all(correct)
        if (is.null(incorrect_msg)) {
          incorrect_msg <- build_incorrect_msg(n_solution_calls = 1, n_args, arg_text, name, functioncalltext)
        }
        expect_that(correct, is_true(), failure_msg = incorrect_msg)
      })
    } 
  } else {
    test_that(sprintf("Function %s()%s is correctly called", name, arg_text), {
      if (n_args > 0) {
        # Only use function calls with the specified arguments
        keep_student <- have_arguments(student_calls, args, ignore, allow_extra)
        student_calls <- student_calls[keep_student]
        keep_solution <- have_arguments(solution_calls, args, ignore, allow_extra)
        solution_calls <- solution_calls[keep_solution]
      }
      n_student_calls <- length(student_calls)
      n_solution_calls <- length(solution_calls)
      
      # Test if there are at least as many student function calls as solution
      # function calls
      if (is.null(not_called_msg)) {
        not_called_msg <- build_not_called_msg(n_solution_calls, name, arg_text, additionaltext)
      }
      expect_that(n_student_calls >= n_solution_calls, is_true(),
                  failure_msg = not_called_msg)
      
      ## If supplied, test arguments
      if (n_args > 0) {
        
        # Loop over the solution function calls:
        # Extract the specified arguments from current solution function call and
        # test if there exists a student function call with the same values
        if (is.null(incorrect_msg)) {
          incorrect_msg <- build_incorrect_msg(n_solution_calls, n_args, arg_text, name, additionaltext)
        }
        incorrect_msg <- rep(incorrect_msg, length.out = n_solution_calls)
        for (i in seq_len(n_solution_calls)) {
          # Extract the specified arguments from current solution function call
          solution_args <- extract_arguments(solution_calls[[i]], args, eval,
                                             env = solution_env)
          
          # Loop over the student function calls:
          # Extract the specified arguments from current student function call
          # and test if they are the same as the values in the solution
          for (j in seq_len(n_student_calls)) {
            student_args <- extract_arguments(student_calls[[j]], args, eval,
                                              env = student_env)
            correct <- mapply(is_equal, student_args, solution_args, eq_condition)
            correct <- all(correct)
            
            if (correct) {
              student_calls[[j]] <- NULL
              n_student_calls <- length(student_calls)
              break
            }
          }
          expect_that(correct, is_true(), failure_msg = incorrect_msg[[i]])
        }
      }
    })
  }
}