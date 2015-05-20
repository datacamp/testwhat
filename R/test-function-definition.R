#' Check whether the student defined a function correctly
#' 
#' @export
test_function_definition <- function(name, 
                                     function_tests = NULL, 
                                     arguments = c(),
                                     check_defaults = TRUE,
                                     body_test = NULL,
                                     student_env = .GlobalEnv,
                                     solution_env = get_solution_env(),
                                     student_code = get_student_code(), 
                                     solution_code = get_solution_code(),
                                     undefined_msg = NULL, 
                                     incorrect_arguments_msg = NULL,
                                     env = parent.frame()) {
  
  body_test <- substitute(body_test)
  if (is.character(body_test)) code <- parse(text = body_test)
  
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Did you define the function <code>%s()</code>?", name)
  }
  
  test_that("Function is defined", {
    expect_that(name, is_defined(env = student_env), failure_msg = undefined_msg)
  })
  
  test_correct({
    # Perform the tests on the function in the student environment
    test_that("Function works as expected", {
      eval(function_tests, envir = student_env)  
    })
  }, {
    # if not correct, go into more detail
    stud_function <- get(name, envir = student_env, inherits = FALSE)
    sol_function <- get(name, envir = solution_env, inherits = FALSE)
    
    # Check for correct definition of arguments
    stud_arguments <- as.list(formals(stud_function))
    sol_arguments <- as.list(formals(sol_function))
    if (is.null(incorrect_arguments_msg)) {
      incorrect_arguments_msg <- sprintf("In the definition of <code>%s()</code>, you did not correctly define the argument%s %s.", 
                                       name, if(length(arguments) == 1) "" else "s", collapse_props(arguments))
    }
    test_that("arguments are correctly defined", {
      for(argument in arguments) {
        expect_that(argument %in% names(stud_arguments), is_true(), failure_msg = incorrect_arguments_msg)
        if(check_defaults) {
          expect_that(sol_arguments[[argument]], equals(stud_arguments[[argument]]), failure_msg = incorrect_arguments_msg)
        }
      }
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