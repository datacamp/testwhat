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
#' @param name  name of the data frame to test.
#' @param columns vector of names of columns to test
#' @param eq_condition  character string indicating how to compare the
#' objects.  Possible values are \code{"equivalent"} (the default),
#' \code{"equal"} and \code{"identical"}.  See \code{\link{is_equivalent_to}},
#' \code{\link{equals}}, and \code{\link{is_identical_to}}, respectively.
#' @param student_env  environment in which the student's code was evaluated.
#' @param solution_env  environment in which the sample solution code was
#' evaluated.
#' @param undefined_msg  feedback message in case the student did not define
#' the data frame or columns.
#' @param incorrect_msg  feedback message in case the columns queried does not correspond between student and solution
#'
#' # Other examples: see SCT design guide
#'
#' @export
test_data_frame <- function(name, columns, eq_condition = "equivalent",
                        student_env = .GlobalEnv,
                        solution_env = get_solution_env(),
                        undefined_msg = NULL, undefined_cols_msg = NULL, incorrect_msg = NULL) {
  
  quoted_name <- paste0("<code>",name,"</code>")
  col_names <- collapse_props(columns)
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Did you define %s?", quoted_name)
  }
  if (is.null(undefined_cols_msg)) {
    undefined_cols_msg <- sprintf("Make sure to specify the columns %s inside %s.", col_names, quoted_name)
  }
  if (is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("It looks like you didn't correctly set one or more of %s inside %s.", col_names, quoted_name)
  }
  
  test_that(sprintf("Object %s is correctly defined", quoted_name), {
    expect_that(name, is_defined(env = student_env),
                failure_msg = undefined_msg)
    student <- get(name, envir = student_env, inherits = FALSE)
    solution <- get(name, envir = solution_env, inherits = FALSE)
    expect_that(columns %in% names(student), is_true(), failure_msg = undefined_cols_msg)
    eq_fun <- switch(eq_condition, equivalent = is_equivalent_to,
                     equal = equals, identical = is_identical_to,
                     stop("invalid equality condition"))
    for(col in columns) {
      expect_that(student[col], eq_fun(solution[col]), failure_msg = incorrect_msg)
    }
  })
}
