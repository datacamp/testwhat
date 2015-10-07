#' Test list elements (or data frame columns)
#'
#' Test whether a student defined a list, and if this is the case,
#' whether the elements of the list correspond to the ones in the solution.
#' A data frame is also a list, so you can use this function to test the
#' correspondence of data frame columns.
#'
#' @param name  name of the list or data frame to test.
#' @param columns character vector or integer vector of list elements or 
#' indices to test.
#' @param eq_condition  character string indicating how to compare the
#' objects. See \code{\link{test_object}}
#' @param student_env environment in which the student's code was evaluated.
#' @param solution_env environment in which the solution code was evaluated.
#' evaluated.
#' @param undefined_msg optional feedback message if list is not defined.
#' @param undefined_cols_msg optional feedback message if not all specified elements 
#' of the solution list were found in the student's list.
#' @param incorrect_msg optional feedback message if not all specified elements of
#' the solution list match those in the student list.
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # df <- data.frame(a = 1:3, b = LETTERS[1:3])
#' 
#' # sct command to test column a
#' test_data_frame("df", columns = "a")
#'
#' # sct command to test column b
#' test_data_frame("df", columns = "b") 
#' }
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

#' @export
test_list_elements <- test_data_frame