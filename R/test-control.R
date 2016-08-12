decorate_state <- function(state, stud, sol, el) {
  state$set(student_pd = stud[[el]]$pd,
            solution_pd = sol[[el]]$pd,
            student_code = stud[[el]]$code,
            solution_code = sol[[el]]$code)
}


#' @export
test_ifelse <- function(state, index = 1, not_found_msg = NULL) {
  test_control(state, index, not_found_msg, type = "if")
}

#' @export
test_while <- function(state, index = 1, not_found_msg = NULL) {
  test_control(state, index, not_found_msg, type = "while")
}

#' @export
test_for <- function(state, index = 1, not_found_msg = NULL) {
  test_control(state, index, not_found_msg, type = "for")
}


test_control <- function(state, index, not_found_msg, type = c("if", "while", "for")) {
  type <- match.arg(type)
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
  control_state <- ControlState$new(state)
  control_state$add_details(type = type,
                            case = "defined",
                            index = index,
                            message = not_found_msg,
                            pd = NULL)

  extract_fun <- switch(type,
                        `if` = extract_if,
                        `for` = extract_for,
                        `while` = extract_while)
  student_structs <- extract_fun(student_pd)
  solution_structs <- extract_fun(solution_pd)
  
  if (length(solution_structs) < index) {
    stop(sprintf("The solution doesn't contain %s %s statements itself.", index, type))
  }
  
  check_that(is_true(length(student_structs) >= index), 
             feedback = control_state$details)
  
  sol_str <- solution_structs[[index]]
  stud_str <- student_structs[[index]]
  
  control_state$set_details(case = "correct",
                            message = NULL,
                            pd = NULL)
  
  control_state$set(student_struct = stud_str,
                    solution_struct = sol_str)
  return(control_state)
}

#' @export
test_cond <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  cond_state <- SubState$new(state)
  cond_state$add_details(type = "condition", pd = student_struct[["cond_part"]]$pd)
  decorate_state(cond_state, student_struct, solution_struct, "cond_part")
  return(cond_state)
}

#' @export
test_body <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  body_state <- SubState$new(state)
  body_state$add_details(type = "body", pd = student_struct[["expr_part"]]$pd)
  decorate_state(body_state, student_struct, solution_struct, "expr_part")
  return(body_state)
}

#' @export
test_if <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  if_state <- SubState$new(state)
  if_state$add_details(type = "ifexpression", pd = student_struct[["if_part"]]$pd)
  decorate_state(if_state, student_struct, solution_struct, "if_part")
  return(if_state)
}

#' @export
test_else <- function(state, not_found_msg = NULL) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  else_state <- SubState$new(state)
  else_state$add_details(type = "elseexpression",
                         case = "defined",
                         message = not_found_msg,
                         pd = state$get("student_pd"))
  
  if (is.null(solution_struct[["else_part"]])) {
    stop(sprintf("The %s control construct in the solution doesn't contain an else part itself.", get_ord(index)))
  }
  
  check_that(is_false(is.null(student_struct[["else_part"]])), 
             feedback = else_state$details)
  
  else_state$set_details(case = "correct",
                         message = NULL,
                         pd = student_struct[["else_part"]]$pd)
  decorate_state(else_state, student_struct, solution_struct, "else_part")
  return(else_state)
}



