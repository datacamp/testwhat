#' Test whether a student correctly called a function (v2)
#'
#' Test whether a student called a function with certain arguments as least as
#' many times as in a sample solution. More details in vignette.
#' 
#' Only difference between V1 and V2 is how \code{index} is treated.
#' 
#' @param index  Which command to check on (both in solution and student).
#' If index is specified, it is checked whether solution and student contain
#' the same amount of function calls.
#' @param index_not_called_msg feedback message in case the queried index wasn't found.
#' @inheritParams test_object
#' @inheritParams test_function

#' @examples
#' \dontrun{
#' # Suppose the solution contains: mean(1:3, na.rm = TRUE)
#' # To test this submission, provide the following in the sct
#' test_function("mean", c("x", "na.rm"))
#' }
#'
#' @import datacampAPI
#' @import testthat
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
  
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
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
      functioncalltext <- build_function_call_text(index)
      # The student call we need is at index for student and solution
      student_calls <- student_calls[index]
      solution_calls <- solution_calls[index]
      
      if (n_args > 0) {
        if (is.null(not_called_msg)) {
          not_called_msg <- build_not_called_msg(n_solution_calls = 1, name, arg_text, functioncalltext)
        }
        # Check if the function is called with the right arguments
        args_ok <- test_what(
          expect_true(have_arguments(student_calls, args, ignore, allow_extra)), 
          not_called_msg
        )
      }
      student_args <- extract_arguments(student_calls[[1]], args, eval, env = student_env)
      solution_args <- extract_arguments(solution_calls[[1]], args, eval, env = solution_env)
      correct <- mapply(is_equal, student_args, solution_args, eq_condition)
      correct <- all(correct)
      if (is.null(incorrect_msg)) {
        incorrect_msg <- build_incorrect_msg(n_solution_calls = 1, n_args, arg_text, name, functioncalltext)
      }
      test_what(expect_true(correct), incorrect_msg)
    } 
  } else {
    arguments <- as.list(match.call())[-1]
    arguments <- arguments[intersect(names(as.list(args(test_function))), names(arguments))]
    
    do.call(test_function, arguments)
  }
}