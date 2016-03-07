#' Test whether a student correctly called a function (v2)
#'
#' Test whether a student called a function with certain arguments as least as
#' many times as in a sample solution.
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
#' @export
test_function_v2 <- function(name, args = NULL, ignore = NULL,
                             allow_extra = TRUE,
                             eval = TRUE,
                             index = 1,
                             eq_condition = "equivalent",
                             not_called_msg = NULL, 
                             incorrect_msg = NULL) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  init_tags(fun = "test_function")
  
  n_args <- length(args)
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  arg_text <- build_arg_text(n_args, args)
  
  # Find all function calls in the student and solution code
  student_calls <- find_function_calls(name, student_pd, student_env)
  solution_calls <- find_function_calls(name, solution_pd, solution_env)
  n_student_calls <- length(student_calls)
  n_solution_calls <- length(solution_calls)
  
  # Check if index exists in solution
  if(index > length(solution_calls)) {
    stop("There aren't %s calls of `%s()` available in the solution.", index, name)
  }
  solution_call <- solution_calls[[index]]
  
  # Test if there are at least as many student function calls as solution
  # function calls
  if (is.null(not_called_msg)) {
    not_called_msg <- build_not_called_msg(n_solution_calls, name, arg_text, additionaltext)
  }
  
  test_what(expect_more_than(n_student_calls+1, n_solution_calls), not_called_msg)
  
  
#   functioncalltext <- build_function_call_text(index, more = length(solution_calls) > 1)
  
#   if (is.null(index_not_called_msg)) {
#     index_not_called_msg <- build_not_enough_calls_text(name, index)
#   }
#   
#   
  # test_what(expect_true(index <= length(student_calls)), incorrect_number_of_calls_msg)
  
  
  for(i in 1:n_student_calls) {
    student_call <- student_calls[[i]]
    
    # test if arguments are called
    if (n_args > 0) {
      if (is.null(not_called_msg)) {
        not_called_msg <- build_not_called_msg(n_solution_calls = 1, name, arg_text, functioncalltext)
      }
      # Check if the function is called with the right arguments
      rep$be_silent()
      run_until_fail(test_what(expect_true(have_arguments_single(student_call, args, ignore, allow_extra)), not_called_msg))
      success <- !rep$get_silent_fail()
      rep$be_loud()
      if(!success) {
        next
      }
    }
    
    # test if arguments are correct
    student_args <- extract_arguments(student_call, args, eval, env = student_env)
    solution_args <- extract_arguments(solution_call, args, eval, env = solution_env)
    
    correct <- mapply(is_equal, student_args, solution_args, eq_condition)
    correct <- all(correct)
    if (is.null(incorrect_msg)) {
      incorrect_msg <- build_incorrect_msg(n_solution_calls = 1, n_args, arg_text, name, functioncalltext)
    }
    
    rep$be_silent()
    run_until_fail(test_what(expect_true(correct), incorrect_msg))
    success <- !rep$get_silent_fail()
    rep$be_loud()
    
  }
}