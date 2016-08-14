test_defined <- function(state, name, undefined_msg, type) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")

  StateClass <- switch(type, object = ObjectState, fundef = FunDefState)
  child_state <- StateClass$new(state)
  child_state$add_details(type = type,
                          case = "defined",
                          name = name,
                          message = undefined_msg,
                          pd = NULL)
  
  check_defined(name, solution_env)
  check_that(is_true(exists(name, envir = student_env, inherits = FALSE)),
             feedback = child_state$details)
  
  child_state$set_details(case = "correct",
                          message = NULL)
  
  child_state$set(student_object = get(name, envir = student_env, inherits = FALSE),
                  solution_object = get(name, envir = solution_env, inherits = FALSE))
  
  return(child_state)
}