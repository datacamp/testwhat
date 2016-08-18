#' Test R object existence and value
#' 
#' Test whether a student defined a certain object. If this is the case, and if 
#' \code{eval} is \code{TRUE}, also check whether the value of the object 
#' matches that of the solution.
#' 
#' @param name name of the object to test.
#' @param eq_condition character string indicating how to compare. See
#'   \code{\link{is_equal}}.
#' @param eval Next to existence, check if the value of the object corresponds 
#'   between student en solution environment.
#' @param undefined_msg Optional feedback message in case the student did not 
#'   define the object. A meaningful message is automatically generated if not 
#'   supplied.
#' @param incorrect_msg optional feedback message in case the student's object 
#'   is not the same as in the sample solution. Only used if \code{eval} is 
#'   \code{TRUE}. A meaningful message is automatically generated if not 
#'   supplied.
#'   
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # x <- mean(1:3, na.rm = TRUE)
#' 
#' # sct command to test existence and value of x:
#' test_object("x")
#' 
#' # sct command to test only existence of x:
#' test_object("x", eval = FALSE)
#' 
#' # Example 2 solution code:
#' # y <- list(a = 2, b = 3, c = 4)
#' 
#' # Small numerical difference allowed + no check on attributes
#' test_object(y)
#' 
#' # Small numerical difference allowed + check attributes
#' test_object(y, eq_condition = "equals")
#' 
#' # No numerical difference allowed + check attributes
#' test_object(y, eq_condtion = "identical")
#' }
#' 
#' @export
test_object <- function(name, eq_condition = "equivalent",
                        eval = TRUE,
                        undefined_msg = NULL, incorrect_msg = NULL) {
  obj_state <- ex() %>% test_obj(name, undefined_msg = undefined_msg)
  if (eval) {
    obj_state %>% test_equal(incorrect_msg = incorrect_msg, eq_condition = eq_condition)  
  }
}


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