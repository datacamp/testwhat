#' @export
test_if_else.State <- function(state, index = 1, not_found_msg) {
  
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
  control_state <- ControlState$new(state)
  control_state$set_details(type = "conditional",
                            case = "defined",
                            index = index)
  
  student_structs <- extract_if(student_pd)
  solution_structs <- extract_if(solution_pd)
  
  if(length(solution_structs) < index) {
    stop(sprintf("The solution doesn't contain %s control constructs itself.", index))
  }
  
  sol_str <- solution_structs[[index]]
  
  check_that(is_true(length(student_structs) >= index), 
             feedback = list(message = not_found_msg,
                             details = state$get("details"),
                             pd = NULL))
  
  stud_str <- student_structs[[index]]
  
  control_state$set(student_struct = stud_str,
                    solution_struct = sol_str)
  return(control_state)
}

# test_cond <- function(state) {
#   student_struct <- state$get("student_struct")
#   solution_struct <- state$get("solution_struct")
#   
#   student_pd <- studnet_struct[[if]]
#   
#   prepare_tw <- function(stud, sol, part) {
#     tw$set(student_pd = stud[[part]][["pd"]])
#     tw$set(solution_pd = sol[[part]][["pd"]])
#     tw$set(student_code = stud[[part]][["code"]])
#     tw$set(solution_code = sol[[part]][["code"]])
#     tw$set(fun_usage = NULL)
#   }
#   
# }


