#' Check whether the student defined a function correctly
#' 
#' @param name  The name of the function to test
#' @param function_test Set of testthat (testwhat) tests to perform on the function
#' @param body_test fill SCT for the function body
#' @param student_env  environment in which the student's code was evaluated.
#' @param solution_env  environment in which the sample solution code was
#' evaluated.
#' @param student_code  character string containing the student's code.
#' @param solution_code  character string containing the sample solution code.
#' @param undefined_msg Feedback message in case the specified function was not defined
#' @param incorrect_number_arguments_msg Feedback message in case the function does not have the correct number of arguments.
#' @param env Environment in which to perform the tests
#' 
#' @export
test_function_definition <- function(name, 
                                     function_test = NULL, 
                                     body_test = NULL,
                                     student_env = .GlobalEnv,
                                     solution_env = get_solution_env(),
                                     student_code = get_student_code(), 
                                     solution_code = get_solution_code(),
                                     undefined_msg = NULL, 
                                     incorrect_number_arguments_msg = NULL,
                                     env = parent.frame()) {
  
  body_test <- substitute(body_test)
  if (is.character(body_test)) code <- parse(text = body_test)
  
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Did you define the function <code>%s()</code>?", name)
  }

  if (is.null(incorrect_number_arguments_msg)) {
    incorrect_number_arguments_msg <- sprintf("Did you specify the correct number of arguments in the function <code>%s()</code>?", name)
  }
  
  test_that("Function is defined", {
    expect_that(name, is_defined(env = student_env), failure_msg = undefined_msg)
  })
  
  test_correct({
    # Perform the tests on the function in the student environment
    test_that("Function works as expected", {
      result <- try(eval(function_test, envir = student_env),silent = TRUE)
      expect_that(inherits(result, "try-error"), is_false(), failure_msg = "Running some tests on your functions generated an error.")
    })
  }, {
    test_that("arguments are correctly defined", {
      # if not correct, go into more detail
      stud_function <- get(name, envir = student_env, inherits = FALSE)
      sol_function <- get(name, envir = solution_env, inherits = FALSE)
      
      # Check for correct definition of arguments
      stud_arguments <- as.list(formals(stud_function))
      sol_arguments <- as.list(formals(sol_function))
      
      expect_that(length(stud_arguments), equals(length(sol_arguments)), failure_msg = incorrect_number_arguments_msg)
    })
  
    # Run SCT code for function body
    if(!is.null(body_test)) {
      set_student_code(paste(deparse(stud_function), collapse = "\n"))
      set_solution_code(paste(deparse(sol_function), collapse = "\n"))
      eval(body_test, envir = env)
      set_student_code(student_code)
      set_solution_code(solution_code)  
    }
  })
}