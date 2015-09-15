#' Test whether a student correctly defined an object
#'
#' Test whether a student defined a certain object and, if yes, test whether
#' that object is the same as in a sample solution.
#'
#' This test is implemented using \code{\link{test_that}}.  Whether the
#' student's object and the sample solution object are the same is tested with
#' \code{\link{is_equivalent_to}}, hence small numeric differences or
#' differences in attributes are allowed.
#'
#' See vignette \code{"testwhat-intro"} for examples.
#'
#' @param name  name of the object to test.
#' @param eq_condition  character string indicating how to compare the
#' objects.  Possible values are \code{"equivalent"} (the default),
#' \code{"equal"} and \code{"identical"}.  See \code{\link{is_equivalent_to}},
#' \code{\link{equals}}, and \code{\link{is_identical_to}}, respectively.
#' @param eval  evaluate the object with name \code{\link{name}} or just check
#' if its exists, without evaluating it.
#' @param student_env  environment in which the student's code was evaluated.
#' @param solution_env  environment in which the sample solution code was
#' evaluated.
#' @param undefined_msg  feedback message in case the student did not define
#' the object.
#' @param incorrect_msg  feedback message in case the student's object is not
#' the same as in the sample solution.
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
test_object <- function(name, eq_condition = "equivalent",
                        eval = TRUE,
                        student_env = .GlobalEnv,
                        solution_env = get_solution_env(),
                        undefined_msg = NULL, incorrect_msg = NULL) {
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
  if (is.null(undefined_msg)) {
    undefined_msg <- build_undefined_object_msg(name)
  }
  if (is.null(incorrect_msg)) {
    incorrect_msg <- build_incorrect_object_msg(name)
  }
  
  test_that(sprintf("Object %s is defined", name), {
    expect_that(name, is_defined(env = student_env),
                failure_msg = undefined_msg)
  })
  
  if (eval) {
    test_that(sprintf("Object %s is correctly defined", name), {
      if (exists(name, envir = solution_env, inherits = FALSE) &&
          exists(name, envir = student_env, inherits = FALSE)) {
        solution <- get(name, envir = solution_env, inherits = FALSE)
        student <- get(name, envir = student_env, inherits = FALSE)
        eq_fun <- switch(eq_condition, equivalent = is_equivalent_to,
                         equal = equals, identical = is_identical_to,
                         stop("invalid equality condition"))
        expect_that(student, eq_fun(solution), failure_msg = incorrect_msg)
      } else {
        fail()
      }
    })
  }
}
