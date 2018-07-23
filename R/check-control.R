#' Check whether student coded a control statement correctly
#' 
#' @param state state to start from (for \code{check_} functions)
#' @param index Number of that particular control statement to check
#' @param not_found_msg Custom message in case the control statement was not found
#' @param append Whether or not to append the feedback to feedback built in previous states
#' @param ... S3 stuff
#' 
#' @examples
#' \dontrun{
#' # Example 1: if else
#' vec <- c("a", "b", "c")
#' if("a" %in% vec) {
#'  print("a in here")
#' } else if(any("b" > vec)) {
#'  cat("b not smallest")
#' } else {
#'  str(vec)
#' }
#'                                            
#' # SCT
#' check_if_else(1) %>%  {
#'   check_cond(.) %>% {
#'     check_code(., "%in%")
#'     check_code(., "vec")
#'   }
#'   check_if(.) %>% check_function(., "print")
#'   check_else(.) %>% check_if_else() {
#'     check_cond(.) %>% check_code(">")
#'     check_if(.) %>% check_function("cat")
#'     check_else(.) %>% check_function("str")
#'   }
#' }
#' 
#' # Example 2: while loop
#' while(x < 18) {
#'  x <- x + 5
#'  print(x)
#' }
#' 
#' # SCT
#' check_while(1) %>% {
#'   check_cond(.) %>% check_code(c("< 18", "18 >"))
#'   check_body(.) %>% {
#'     check_code(., c("x + 5", "5 + x"))
#'     check_function(., "print") %>% test_arg("x")
#'   }
#' }
#' 
#' # Example 3: for loop
#' for(i in 1:5) {
#'  print("hurray!") 
#' }
#' 
#' # SCT
#' ex() %>% check_for() %>% {
#'   check_cond(.) %>% {
#'     check_code(., "in")
#'     check_code(., "1")
#'     check_code(., "5")
#'   }
#'   check_body(.) %>% check_function("print") %>% check_arg("x") %>% check_equal()
#' }
#' }
#' @name check_control

#' @rdname check_control
#' @export
check_if_else <- function(state, index = 1, not_found_msg = NULL, append = TRUE) {
  check_control(state, index, not_found_msg, append = append, type = "if")
}

#' @rdname check_control
#' @export
check_while <- function(state, index = 1, not_found_msg = NULL, append = TRUE) {
  check_control(state, index, not_found_msg, append = append, type = "while")
}

#' @rdname check_control
#' @export
check_for <- function(state, index = 1, not_found_msg = NULL, append = TRUE) {
  check_control(state, index, not_found_msg, append = append, type = "for")
}


check_control <- function(state, index, not_found_msg, append, type = c("if", "while", "for")) {
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
                            append = append,
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

#' @rdname check_control
#' @export
check_cond <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  cond_state <- SubState$new(state)
  cond_state$add_details(type = "condition", pd = student_struct[["cond_part"]]$pd, append = TRUE)
  decorate_state(cond_state, student_struct, solution_struct, "cond_part")
  return(cond_state)
}

#' @rdname check_control
#' @export
check_body.ControlState <- function(state, ...) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  body_state <- SubState$new(state)
  body_state$add_details(type = "body", pd = student_struct[["expr_part"]]$pd, append = TRUE)
  decorate_state(body_state, student_struct, solution_struct, "expr_part")
  return(body_state)
}

#' @rdname check_control
#' @export
check_if <- function(state) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  if_state <- SubState$new(state)
  if_state$add_details(type = "ifexpression", pd = student_struct[["if_part"]]$pd, append = TRUE)
  decorate_state(if_state, student_struct, solution_struct, "if_part")
  return(if_state)
}

#' @rdname check_control
#' @export
check_else <- function(state, not_found_msg = NULL, append = TRUE) {
  student_struct <- state$get("student_struct")
  solution_struct <- state$get("solution_struct")
  else_state <- SubState$new(state)
  else_state$add_details(type = "elseexpression",
                         case = "defined",
                         message = not_found_msg,
                         append = append,
                         pd = state$get("student_pd"))
  
  if (is.null(solution_struct[["else_part"]])) {
    stop(sprintf("The control construct in the solution doesn't contain an else part itself."))
  }
  
  check_that(is_false(is.null(student_struct[["else_part"]])), 
             feedback = else_state$details)
  
  else_state$set_details(case = "correct",
                         message = NULL,
                         pd = student_struct[["else_part"]]$pd)
  decorate_state(else_state, student_struct, solution_struct, "else_part")
  return(else_state)
}

# Deprecated functions

test_if_else <- function(index = 1, 
                         if_cond_test = NULL, 
                         if_expr_test = NULL, 
                         else_expr_test = NULL,
                         not_found_msg = NULL,
                         missing_else_msg = NULL) {
  fail_if_v2_only()
  old_state <- ex()  
  test_env <- old_state$get("test_env")
  on.exit(tw$set(state = old_state))
  
  testif <- old_state %>% check_if_else(index = index, not_found_msg = not_found_msg, append = is.null(not_found_msg))
  
  if_cond_test <- substitute(if_cond_test)
  if (!is.null(if_cond_test)) {
    cond_state <- testif %>% check_cond()
    tw$set(state = cond_state)
    eval(if_cond_test, envir = test_env)
  }
  
  if_expr_test <- substitute(if_expr_test)
  if (!is.null(if_expr_test)) {
    ifexprstate <- testif %>% check_if()
    tw$set(state = ifexprstate)
    eval(if_expr_test, envir = test_env)
  }
  
  else_expr_test <- substitute(else_expr_test)
  if (!is.null(else_expr_test)) {
    elseexprstate <- testif %>% check_else(not_found_msg = missing_else_msg, append = is.null(missing_else_msg))
    tw$set(state = elseexprstate)
    eval(else_expr_test, envir = test_env)
  }
}

test_while_loop <- function(index = 1, 
                            cond_test = NULL, 
                            expr_test = NULL,                          
                            not_found_msg = NULL) {
  fail_if_v2_only()
  cond_test <- substitute(cond_test)
  expr_test <- substitute(expr_test)
  test_loop(index = index, 
            cond_test = cond_test,
            expr_test = expr_test,
            not_found_msg = not_found_msg,
            fun = check_while)
}

test_for_loop <- function(index = 1, 
                          cond_test = NULL,
                          expr_test = NULL,
                          not_found_msg = NULL) {
  fail_if_v2_only()
  cond_test <- substitute(cond_test)
  expr_test <- substitute(expr_test)
  test_loop(index = index, 
            cond_test = cond_test,
            expr_test = expr_test,
            not_found_msg = not_found_msg,
            fun = check_for)
}

test_loop <- function(index, cond_test, expr_test, not_found_msg, fun) {
  old_state <- ex()
  test_env <- old_state$get("test_env")
  on.exit(tw$set(state = old_state))
  
  testloop <- old_state %>% fun(index = index, not_found_msg = not_found_msg, append = is.null(not_found_msg))
  
  if (!is.null(cond_test)) {
    cond_state <- testloop %>% check_cond()
    tw$set(state = cond_state)
    eval(cond_test, envir = test_env)
  }
  
  if (!is.null(expr_test)) {
    body_state <- testloop %>% check_body()
    tw$set(state = body_state)
    eval(expr_test, envir = test_env)
  }
}



