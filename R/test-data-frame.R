#' Test whether a student correctly defined the column(s) of a data.frame
#'
#' Test whether a student defined a data.frame, and, if see, whether the columns of this data.frame
#' correspond to the sample solution.
#'
#' This test is implemented using \code{\link{test_what}}.  Whether the
#' student's column and the sample solution object are the same is tested with
#' \code{\link{is_equivalent_to}}, hence small numeric differences or
#' differences in attributes are allowed.
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
#' the data frame.
#' @param undefined_cols_msg feedback message in case the student did define one or more fo the columns inside the data frame
#' @param incorrect_msg  feedback message in case the columns queried does not correspond between student and solution
#'
#' # Other examples: see SCT design guide
#'
#' @export
test_data_frame <- function(name, columns = NULL, 
                            eq_condition = "equivalent",
                            student_env = .GlobalEnv,
                            solution_env = get_solution_env(),
                            undefined_msg = NULL, 
                            undefined_cols_msg = NULL, 
                            incorrect_msg = NULL) {
  
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
  if (!exists(name, solution_env)) {
    stop(sprintf("%s is not defined in your solution environment.", name))
  }
  
  solution <- get(name, envir = solution_env, inherits = FALSE)
  
  if (is.null(columns)) {
    columns <- names(get(name, envir = solution_env, inherits = FALSE))
  }
  
  quoted_name <- paste0("<code>",name,"</code>")
  col_names <- collapse_props(columns)
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Did you define %s?", quoted_name)
  }
  if (is.null(undefined_cols_msg)) {
    undefined_cols_msg <- sprintf("Make sure to specify the column%s %s inside %s.", if(length(columns) > 1) "s" else "", col_names, quoted_name)
  }
  if (is.null(incorrect_msg)) {
    if(length(columns) == 1) {
      incorrect_msg <- sprintf("It looks like you didn't correctly set the column %s inside %s.", col_names, quoted_name)  
    } else {
      incorrect_msg <- sprintf("It looks like you didn't correctly set one or more of the columns %s inside %s.", col_names, quoted_name)  
    }
  }
  
  defined <- test_what(expect_defined(name, student_env), undefined_msg)
  if (defined) {
    student <- get(name, envir = student_env, inherits = FALSE)
    
    columns_defined <- test_what(expect_true(all(columns %in% names(student))), undefined_cols_msg)
    
    if (columns_defined) {
      eq_fun <- switch(eq_condition, 
                       equivalent = expect_equivalent,
                       equal = expect_equal, 
                       identical = expect_identical,
                       stop("invalid equality condition"))
      
      for(col in columns) {
        test_what(eq_fun(student[col], solution[col]), incorrect_msg)
      }
    }
  }
}
