test_equal <- function(state, ...) UseMethod("test_equal", state)

test_equal.default <- function(state, ...) {
  stop("Can't run test_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

test_equal.ObjectState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$set_details(case = "equal",
                    student = student_obj,
                    solution = solution_obj,
                    eq_condition = eq_condition)
  
  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = list(message = incorrect_msg,
                             details = state$get("details"),
                             pd = state$get("student_pd")))
  return(state)
}

