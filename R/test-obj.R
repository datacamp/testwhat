#' Test R object existence
#' 
#' @export
test_obj <- function(state, name, undefined_msg = NULL) {
  
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  obj_state <- ObjectState$new(state)
  obj_state$set(object_name = name)
  obj_state$add_details(type = "object",
                        case = "defined",
                        name = name)

  check_defined(name, solution_env)
  solution_object <- get(name, envir = solution_env, inherits = FALSE)
  
  check_that(is_true(exists(name, envir = student_env, inherits = FALSE)),
             feedback = list(message = undefined_msg,
                             details = obj_state$get("details"),
                             pd = NULL))
  
  student_object <- get(name, envir = student_env, inherits = FALSE)
  
  obj_state$set(student_object = student_object,
                solution_object = solution_object,
                student_pd = extract_object_assignment(student_pd, name),
                solution_pd = extract_object_assignment(solution_pd, name))
  return(obj_state)
}

test_equal <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
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