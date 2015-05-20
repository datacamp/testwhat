#' Check whether the student wrote a while loop correctly
#' 
#' @export
test_while_loop <- function(index = 1, cond_test, expr_test,                          
                            student_code = get_student_code(), 
                            solution_code = get_solution_code(),
                            not_found_msg = NULL,
                            env = parent.frame()) {
  
  cond_test <- substitute(cond_test)
  if (is.character(cond_test)) code <- parse(text = cond_test)
  
  expr_test <- substitute(expr_test)
  if (is.character(expr_test)) expr_test <- parse(text = expr_test)
  
  stud_pd <- getParseData(parse(text = paste(get_clean_lines(student_code), collapse = "\n")))
  student_whiles <- extract_while_wrapper(0, stud_pd)
  sol_pd <- getParseData(parse(text = paste(get_clean_lines(solution_code), collapse = "\n")))
  solution_whiles <- extract_while_wrapper(0, sol_pd)
  
  if(is.null(not_found_msg)) {
    not_found_msg <- sprintf(paste("The system wants to test if the %s while loop",
                                   "you coded is correct, but it hasn't found it. Add more code."), 
                             get_num(index))
  }
  
  ok = test_sufficient_length(student_whiles, index, 
                              incorrect_number_of_calls_msg = not_found_msg)
  if(isTRUE(ok)) {
    stud_while <- student_whiles[[index]]
    sol_while <- solution_whiles[[index]]
    additionaltext <- sprintf(" in the %s while loop of your submission", get_num(index))
  } else {
    return(FALSE)
  }
  
  on.exit({
    set_student_code(student_code)
    set_solution_code(solution_code)
  })
  
  # WHILE condition part should always be there
  test_that("while_cond is available", {
    expect_that(is.null(stud_while$while_cond), is_false(), failure_msg = sprintf("The <code>condition</code> part%s is missing.", additionaltext))  
  })
  if(!is.null(cond_test) && !is.null(stud_while$while_cond) && !is.null(sol_while$while_cond)) {
    set_student_code(stud_while$while_cond)
    set_solution_code(sol_while$while_cond)
    eval(cond_test, envir = env)
  }
  
  # IF expression part should always be available.
  test_that("while_expr is available", {
    expect_that(is.null(stud_while$while_expr), is_false(), failure_msg = sprintf("The <code>expr</code> part%s is missing.", additionaltext))  
  })
  if(!is.null(expr_test) && !is.null(stud_while$while_expr) && !is.null(sol_while$while_expr)) {
    set_student_code(stud_while$while_expr)
    set_solution_code(sol_while$while_expr)
    eval(expr_test, envir = env)
  }
}

extract_while_wrapper <- function(parent_id, pd) {
  if(any(pd$token == "WHILE")) {
    top_ids <- pd$id[pd$parent == parent_id]
    structs <- lapply(top_ids, extract_while, pd)
    structs <- structs[!sapply(structs, is.null)]  
    if(length(structs) == 0) {
      return(unlist(lapply(top_ids, extract_while, pd), recursive = FALSE))
    } else {
      return(structs)
    }
  } else {
    return(list())
  }
}

extract_while <- function(parent_id, pd) {
  if(length(pd$id[pd$token == "WHILE" & pd$parent == parent_id]) == 0) {
    return(NULL)
  }
  
  while_parts <- pd$id[pd$token == "expr" & pd$parent == parent_id]
  
  while_cond <- getParseText(pd, while_parts[1])
  while_expr <- getParseText(pd, while_parts[2])
  
  return(list(while_cond = while_cond, while_expr = while_expr))
}
