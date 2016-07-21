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
#' @param undefined_msg optional feedback message if list is not defined.
#' @param undefined_cols_msg optional feedback message if not all specified elements 
#' of the solution list were found in the student's list.
#' @param incorrect_msg optional feedback message if not all specified elements of
#' the solution list match those in the student list.
#' @inheritParams test_object
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
                            undefined_msg = NULL, 
                            undefined_cols_msg = NULL, 
                            incorrect_msg = NULL) {
  
  # Get needed elements from tw
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  init_tags(fun = "test_data_frame")
  
  check_defined(name, solution_env)
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
  defined <- exists(name, envir = student_env, inherits = FALSE)
  check_that(is_true(defined), undefined_msg)
  if (defined) {
    student <- get(name, envir = student_env, inherits = FALSE)
    
    columns_defined <- all(columns %in% names(student))
    check_that(is_true(columns_defined), undefined_cols_msg)
    
    if (columns_defined) {
      for(col in columns) {
        check_that(is_equal(student[col], solution[col], eq_condition), feedback = incorrect_msg)
      }
    }
  }
}
