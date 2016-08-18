#' Test R object existence
#'
#' @rdname test_object
#' @export
test_obj <- function(state, name, undefined_msg = NULL) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  obj_state <- ObjectState$new(state)
  obj_state$add_details(type = "object",
                        case = "defined",
                        name = name,
                        message = undefined_msg,
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

#' @export
test_col <- function(state, col, col_missing_msg = NULL) {
  test_sub_helper(state, sub = col, sub_missing_msg = col_missing_msg, type = "column")
}

#' @export
test_el <- function(state, el, el_missing_msg = NULL) {
  test_sub_helper(state, sub = el, sub_missing_msg = el_missing_msg, type = "element")
}


## HELPERS

test_sub_helper <- function(state, sub, sub_missing_msg = NULL, type = c("column", "element")) {
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
                               message = sub_missing_msg)
  
  check_that(is_true(sub %in% names(student_object)), feedback = object_sub_state$details)
  
  object_sub_state$set_details(case = "correct",
                               message = NULL)
  
  object_sub_state$set(student_object = student_object[[sub]],
                       solution_object = solution_object[[sub]])
  
  return(object_sub_state)
}

test_equal_helper <- function(state, incorrect_msg, eq_condition, type = c("object", "column", "element")) {
  type <- match.arg(type)
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$add_details(type = type,
                    case = "equal",
                    student = student_obj,
                    solution = solution_obj,
                    eq_condition = eq_condition,
                    message = incorrect_msg)
  
  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = state$details)
  return(state)
}

