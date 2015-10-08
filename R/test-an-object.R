#' Check if the student defined an object, independent of the name
#' 
#' This function is an adaption of \code{\link{test_object}}. The function will
#' check if a specific object, defined in the solution, exists. The object the 
#' student defined doesn't have to have the same name. In other words,
#' this function will check if any defined variable by the user corresponds to
#' a specific variable in the solution code. 
#'
#' @param name  name of object in solution to test.
#' @param undefined_msg feedback message in case the student did not define 
#' an object that corresponds to the solution object. This argument should 
#' always be specified.
#' @inheritParams test_object
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # x <- 5
#' 
#' # sct command to test whether student defined _an_ object with same value
#' test_an_object("x")
#' 
#' # All of the following student submissions are accepted
#' # x <- 5
#' # y <- 5
#' # z <- 4 + 1 + 1e-8
#' }
#' 
#' @import datacampAPI
#' @import testthat
#' @export
test_an_object <- function(name, undefined_msg,
                        eq_condition = "equivalent",
                        student_env = .GlobalEnv,
                        solution_env = get_solution_env()) {
  
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
  if (is.null(undefined_msg)) {
    stop("argument \"undefined\" is missing, with no default")
  }
  
  if (!exists(name, solution_env)) {
    stop(sprintf("%s is not defined in your solution environment.", name))
  }
  
  solution <- get(name, envir = solution_env, inherits = FALSE)
    
  eq_fun <- switch(eq_condition, equivalent = .equivalent, 
                                 equal = .equal,
                                 identical = identical, 
                                 stop("invalid equality condition"))
    
  valid_values <- list()
  length(valid_values) <- length(ls(student_env))
  
  counter <- 1
  for (student_var in ls(student_env)) {
    student_value <- get(student_var, envir = student_env, inherits = FALSE)
    if (identical(class(student_value), class(solution))) {
      valid_values[[counter]] <- student_value
      counter <- counter + 1
    }
  }
  
  if (counter > 1) {
    correct <- vapply(valid_values[1:counter-1], function(x) { eq_fun(x, solution) }, logical(1))
  } else {
    correct <- FALSE
  }
  
  test_what(expect_true(any(correct)), undefined_msg)
}

