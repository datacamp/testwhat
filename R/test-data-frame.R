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
  obj_state <- ex() %>% test_obj(name = name, undefined_msg = undefined_msg)
  
  if (is.null(columns)) {
    columns <- names(obj_state$get("solution_object"))
  }
  
  for (col in columns) {
    obj_state %>% 
      test_col(col, col_missing_msg = undefined_cols_msg) %>% 
      test_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg)
  }
}

test_col <- function(state, col, col_missing_msg = NULL) {
  student_object <- state$get("student_object")
  solution_object <- state$get("solution_object")
  
  if (!col %in% names(solution_object)) {
    stop(sprintf("The column %s is not available", col))
  }
  
  col_state <- ObjectState$new(state)
  col_state$add_details(type = "column",
                        case = "defined",
                        name = col,
                        message = col_missing_msg)
  
  check_that(is_true(col %in% names(student_object)), feedback = col_state$details)
  
  col_state$set_details(type = "column",
                        case = "correct",
                        message = NULL)
  
  col_state$set(student_object = student_object[[col]],
                solution_object = solution_object[[col]])
  
  return(col_state)
}


#' @export
test_equal.ObjectSubState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  test_equal_helper(state, incorrect_msg = incorrect_msg, eq_condition = eq_condition, type = "column")
}

