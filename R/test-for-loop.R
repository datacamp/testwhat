#' Check whether the student wrote a for loop correctly
#' 
#' @param index  The index of the for loop to check.
#' @param cond_test  SCT to perform on the condition part of the for loop
#' @param expr_test  SCT to perform on the expression part of the for loop
#' @param student_code  character string containing the student's code.
#' @param solution_code  character string containing the sample solution code.
#' @param not_found_msg  Message in case the for loop (at given index) is not found.
#' @param env  Environment in which to perform all these SCTs
#' 
#' @export
test_for_loop <- function(index = 1, 
                          cond_test = NULL,
                          expr_test = NULL,                          
                          student_code = get_student_code(), 
                          solution_code = get_solution_code(),
                          not_found_msg = NULL,
                          env = parent.frame()) {
  
  var_test <- substitute(var_test)
  if (is.character(var_test)) code <- parse(text = var_test)
  
  seq_test <- substitute(seq_test)
  if (is.character(seq_test)) code <- parse(text = seq_test)
  
  expr_test <- substitute(expr_test)
  if (is.character(expr_test)) expr_test <- parse(text = expr_test)
  
  stud_pd <- getParseData(parse(text = paste(get_clean_lines(student_code), collapse = "\n")))
  student_fors <- extract_for_wrapper(0, stud_pd)
  sol_pd <- getParseData(parse(text = paste(get_clean_lines(solution_code), collapse = "\n")))
  solution_fors <- extract_for_wrapper(0, sol_pd)
  
  if(is.null(not_found_msg)) {
    not_found_msg <- sprintf(paste("The system wants to test if the %s <code>for</code> loop",
                                   "you coded is correct, but it hasn't found it. Add more code."), 
                             get_num(index))
  }
  
  ok = test_sufficient_length(student_fors, index, 
                              incorrect_number_of_calls_msg = not_found_msg)
  if(isTRUE(ok)) {
    stud_for <- student_fors[[index]]
    sol_for <- solution_fors[[index]]
    additionaltext <- sprintf(" in the %s <code>for</code> loop of your submission", get_num(index))
  } else {
    return(FALSE)
  }
  
  on.exit({
    set_student_code(student_code)
    set_solution_code(solution_code)
  })
  
  # for var part should always be there
  test_what(expect_false(is.null(stud_for$for_cond)), sprintf("The <code>condition</code> part%s is missing.", additionaltext))
  if(!is.null(cond_test) && !is.null(stud_for$for_cond) && !is.null(sol_for$for_cond)) {
    set_student_code(stud_for$for_cond)
    set_solution_code(sol_for$for_cond)
    eval(cond_test, envir = env)
  }

  # IF expression part should always be available.
  test_what(expect_false(is.null(stud_for$for_expr)), sprintf("The <code>expr</code> part%s is missing.", additionaltext))  

  if(!is.null(expr_test) && !is.null(stud_for$for_expr) && !is.null(sol_for$for_expr)) {
    set_student_code(stud_for$for_expr)
    set_solution_code(sol_for$for_expr)
    eval(expr_test, envir = env)
  }
}

extract_for_wrapper <- function(parent_id, pd) {
  if(any(pd$token == "FOR")) {
    top_ids <- pd$id[pd$parent == parent_id]
    structs <- lapply(top_ids, extract_for, pd)
    structs <- structs[!sapply(structs, is.null)]  
    if(length(structs) == 0) {
      return(unlist(lapply(top_ids, extract_for, pd), recursive = FALSE))
    } else {
      return(structs)
    }
  } else {
    return(list())
  }
}

extract_for <- function(parent_id, pd) {
  if(length(pd$id[pd$token == "FOR" & pd$parent == parent_id]) == 0) {
    return(NULL)
  }
  
  for_parts <- pd$id[(pd$token == "expr" | pd$token == "forcond") & pd$parent == parent_id]
  
  for_cond <- getParseText(pd, for_parts[1])
  for_expr <- getParseText(pd, for_parts[2])  
  
  return(list(for_cond = for_cond, for_expr = for_expr))
}
