#' @export
test_ifelse <- function(state, index = 1, not_found_msg = NULL) {
  
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
  control_state <- ControlState$new(state)
  control_state$add_details(type = "ifelse",
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
                             details = control_state$get("details"),
                             pd = NULL))
  
  stud_str <- student_structs[[index]]
  
  control_state$set(student_struct = stud_str,
                    solution_struct = sol_str)
  return(control_state)
}

decorate_state <- function(state, stud, sol, el) {
  state$set(student_pd = stud[[el]]$pd,
            solution_pd = sol[[el]]$pd,
            student_code = stud[[el]]$code,
            solution_code = sol[[el]]$code)
}

#' @export
test_cond <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  cond_state <- SubState$new(state)
  cond_state$set_details(type = "ifcondition")
  decorate_state(cond_state, student_struct, solution_struct, "cond_part")
  return(cond_state)
}

#' @export
test_if <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  if_state <- SubState$new(state)
  if_state$set_details(type = "ifexpression")
  decorate_state(if_state, student_struct, solution_struct, "if_part")
  return(if_state)
}

#' @export
test_else <- function(state, missing_else_msg = NULL) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  else_state <- SubState$new(state)
  else_state$set_details(type = "elseexpression", case = "defined")
  
  if (is.null(solution_struct[["else_part"]])) {
    stop(sprintf("The %s control construct in the solution doesn't contain an else part itself.", get_ord(index)))
  }
  
  check_that(is_false(is.null(student_struct[["else_part"]])), 
             feedback = list(message = missing_else_msg,
                             details = else_state$get("details"),
                             pd = state$get("student_pd")))
  
  else_state$set_details(type = "elseexpression", case = "correct")
  decorate_state(else_state, student_struct, solution_struct, "else_part")
  return(else_state)
}


