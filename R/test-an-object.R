#' This function is an adaption of \code{\link{test_object}}. The function will
#' check if a specific object, defined in the solution, exists. The difference
#' here is that the object does not require the same variable name. In other words,
#' this function will check if any defined variable by the user corresponds to
#' a specific variable in the solution code.
#'
#' This test is implemented using \code{\link{test_that}}.  Whether the
#' student's object and the sample solution object are the same is tested with
#' \code{\link{is_equivalent_to}}, hence small numeric differences or
#' differences in attributes are allowed.
#'
#' See vignette \code{"testwhat-intro"} for examples.
#'
#' @param name  name of the object to test.
#' @param undefined_msg  feedback message in case the student did not define 
#' an object which corresponds to the solution object. This parameter should 
#' always be used by the sct designer, since the function is used in very
#' specific cases.
#' @param eq_condition  character string indicating how to compare the
#' objects.  Possible values are \code{"equivalent"} (the default),
#' \code{"equal"} and \code{"identical"}.  See \code{\link{is_equivalent_to}},
#' \code{\link{equals}}, and \code{\link{is_identical_to}}, respectively.
#' @param student_env  environment in which the student's code was evaluated.
#' @param solution_env  environment in which the sample solution code was
#' evaluated.
#' 
#' @examples
#' \dontrun{
#' # Suppose the solution contains: x <- mean(1:3, na.rm = TRUE)
#' # To test this submission, provide the following in the sct
#' test_function("x")
#' }
#'
#' # Other examples: see SCT design guide
#'
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

