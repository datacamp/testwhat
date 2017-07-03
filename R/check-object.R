#' Test R object existence and value
#' 
#' Test whether a student defined a certain object (correctly)
#' 
#' @param state the state to start from
#' @param name name of the object to test.
#' @param eq_condition character string indicating how to compare. See 
#'   \code{\link{is_equal}}.
#' @param eval Next to existence, check if the value of the object corresponds 
#'   between student en solution environment
#' @param undefined_msg Optional feedback message in case the student did not 
#'   define the object. A meaningful message is automatically generated if not 
#'   supplied.
#' @param incorrect_msg Custom feedback message in case the student's object is
#'   not the same as in the sample solution.
#' @param columns character vector of data frame columns to test
#' @param undefined_cols_msg Custom feedback message if not all specified
#'   columns of were found in the student's data frame.
#' @param col_missing_msg Custom message in case data frame column is missing
#' @param col name of column to check
#' @param el_missing_msg Custom message in case element is messing.
#' @param el name of element to check
#' @param append Whether or not to append the feedback to feedback built in previous states
#' @param ... S3 stuff
#' 
#' @examples
#' \dontrun{
#' # Example 1
#' x <- mean(1:3, na.rm = TRUE)
#' 
#' # SCT option 1
#' test_object("x")
#' 
#' # SCT option 2 
#' ex() %>% check_object("x") %>% check_equal()
#' 
#' # Example 2
#' x <- mean(1:3, na.rm = TRUE)
#' 
#' # SCT option 1 to only check existence of x
#' test_object("x", eval = FALSE)
#' 
#' # SCT option 2 to only check existence of x
#' ex() %>% check_object("x")
#' 
#' # Example 3
#' df <- data.frame(a = 1:3, b = LETTERS[1:3])
#' 
#' # SCT option 1 to test column a
#' test_data_frame("df", columns = "a")
#'
#' # SCT option 2 to test column a
#' ex() %>% check_object("df") %>% check_column("a") %>% check_equal()
#' 
#' # Example 4
#' lst <- list(a = 1, b = 2)
#' 
#' # SCT to test only element b
#' ex() %>% check_object("lst") %>% check_element("b") %>% check_equal()
#' }
#' @name test_object


#' @rdname test_object
#' @export
test_object <- function(name, eq_condition = "equivalent",
                        eval = TRUE,
                        undefined_msg = NULL, incorrect_msg = NULL) {
  obj_state <- ex() %>% check_object(name, 
                                     undefined_msg = undefined_msg,
                                     append = is.null(undefined_msg))
  if (eval) {
    obj_state %>% check_equal(incorrect_msg = incorrect_msg, 
                              eq_condition = eq_condition,
                              append = is.null(incorrect_msg))
  }
}


#' @rdname test_object
#' @export
test_data_frame <- function(name, columns = NULL, 
                            eq_condition = "equivalent",
                            undefined_msg = NULL,
                            undefined_cols_msg = NULL, 
                            incorrect_msg = NULL) {
  obj_state <- ex() %>% check_object(name = name, 
                                     undefined_msg = undefined_msg,
                                     append = is.null(undefined_msg))
  
  if (is.null(columns)) {
    columns <- names(obj_state$get("solution_object"))
  }
  
  for (col in columns) {
    obj_state %>% 
      check_column(col, col_missing_msg = undefined_cols_msg, append = is.null(undefined_cols_msg)) %>% 
      check_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg, append = is.null(incorrect_msg))
  }
}

#' @rdname test_object
#' @export
check_object <- function(state, name, undefined_msg = NULL, append = TRUE) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  assert_is_string(name)
  
  obj_state <- ObjectState$new(state)
  obj_state$add_details(type = "object",
                        case = "defined",
                        name = name,
                        message = undefined_msg,
                        append = append,
                        pd = NULL)
  
  check_defined(name, solution_env)
  check_that(is_true(exists(name, envir = student_env, inherits = FALSE)),
             feedback = obj_state$details)
  
  obj_state$set_details(case = "correct",
                        message = NULL,
                        pd = extract_object_assignment(state$get("student_pd"), name))
  
  obj_state$set(student_object = get(name, envir = student_env, inherits = FALSE),
                solution_object = get(name, envir = solution_env, inherits = FALSE))
  
  return(obj_state)
}

#' @rdname test_object
#' @export
check_column <- function(state, col, col_missing_msg = NULL, append = TRUE) {
  check_sub_helper(state, sub = col, sub_missing_msg = col_missing_msg, append = append, type = "column")
}

#' @rdname test_object 
#' @export
check_element <- function(state, el, el_missing_msg = NULL, append = TRUE) {
  check_sub_helper(state, sub = el, sub_missing_msg = el_missing_msg, append = append, type = "element")
}


check_sub_helper <- function(state, sub, sub_missing_msg, append, type = c("column", "element")) {
  assert_is_string(sub)
  type <- match.arg(type)
  ObjectSubState <- switch(type, column = ObjectColumnState, element = ObjectElementState)
  student_object <- state$get("student_object")
  solution_object <- state$get("solution_object")
  
  if (!sub %in% names(solution_object)) {
    stop(sprintf("The %s %s is not available", type, sub))
  }
  
  object_sub_state <- ObjectSubState$new(state)
  object_sub_state$add_details(type = type,
                               case = "defined",
                               name = sub,
                               message = sub_missing_msg,
                               append = append)
  
  check_that(is_true(sub %in% names(student_object)), feedback = object_sub_state$details)
  
  object_sub_state$set_details(case = "correct",
                               message = NULL)
  
  object_sub_state$set(student_object = student_object[[sub]],
                       solution_object = solution_object[[sub]])
  
  return(object_sub_state)
}

#' @rdname test_object
#' @export
check_equal.ObjectState <- function(state, incorrect_msg = NULL, append = TRUE, eq_condition = "equivalent", ...) {
  check_equal_helper(state, incorrect_msg = incorrect_msg, append = append, eq_condition = eq_condition, type = "object")
}

#' @rdname test_object
#' @export
check_equal.ObjectColumnState <- function(state, incorrect_msg = NULL, append = TRUE, eq_condition = "equivalent", ...) {
  check_equal_helper(state, incorrect_msg = incorrect_msg, append = append, eq_condition = eq_condition, type = "column")
}

#' @rdname test_object
#' @export
check_equal.ObjectElementState <- function(state, incorrect_msg = NULL, append = TRUE, eq_condition = "equivalent", ...) {
  check_equal_helper(state, incorrect_msg = incorrect_msg, append = append, eq_condition = eq_condition, type = "element")
}

check_equal_helper <- function(state, incorrect_msg, eq_condition, append, type = c("object", "column", "element"), ...) {
  type <- match.arg(type)
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$add_details(type = type,
                    case = "equal",
                    student = student_obj,
                    solution = solution_obj,
                    eq_condition = eq_condition,
                    message = incorrect_msg,
                    append = append)
  
  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = state$details)
  return(state)
}
