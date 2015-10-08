#' Test a while loop
#' 
#' Test whether a student correctly coded a while loop. The function parses
#' the student and solution code and selects the first, second ... while loop in the
#' code depending on the \code{index} argument, and then runs two chunks of tests:
#' \itemize{
#'  \item{\code{cond_test}: \code{testwhat} tests specifically for
#'  the condition part of the while loop.}
#'  \item{\code{expr_test}: \code{testwhat} tests for the code inside the while loop}
#' }
#' The tests for the conditional part and the expression part
#' of the while loop can only be text-based. You cannot use functions such
#' as \code{\link{test_object}} that also depend on the student and solution
#' environment.
#' 
#' @param index  The index of the while loop to check.
#' @param cond_test  SCT to perform on the condition part of the while loop
#' @param expr_test  SCT to perform on the expression part of the while loop
#' @param not_found_msg  Message in case the while loop (at given index) is not found.
#' @param env  Environment in which to perform all these SCTs
#' @inheritParams test_object
#' 
#' 
#' #' @examples
#' \dontrun{
#' # Example solution code:
#' while(x < 18) {
#'  x <- x + 5
#'  print(x)
#' }
#' 
#' # SCT to test this loop:
#' test_while_loop({
#'  test_student_typed(c("< 18", "18 >"))
#' }, {
#'  test_student_Typed(c("x + 5", "5 = x"))
#'  test_function("print", eval = FALSE) # no actual value matching possible!!
#' })
#' }
#' 
#' @export
test_while_loop <- function(index = 1, 
                            cond_test = NULL, 
                            expr_test = NULL,                          
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
    not_found_msg <- sprintf(paste("The system wants to test if the %s <code>while</code> loop",
                                   "you coded is correct, but it hasn't found it. Add more code."), 
                             get_num(index))
  }
  
  ok = test_sufficient_length(student_whiles, index, 
                              incorrect_number_of_calls_msg = not_found_msg)
  if(isTRUE(ok)) {
    stud_while <- student_whiles[[index]]
    sol_while <- solution_whiles[[index]]
    additionaltext <- sprintf(" in the %s <code>while</code> loop of your submission", get_num(index))
  } else {
    return(FALSE)
  }
  
  on.exit({
    set_student_code(student_code)
    set_solution_code(solution_code)
  })
  
  # WHILE condition part should always be there
  test_what(expect_false(is.null(stud_while$while_cond)), sprintf("The <code>condition</code> part%s is missing.", additionaltext))
  if(!is.null(cond_test) && !is.null(stud_while$while_cond) && !is.null(sol_while$while_cond)) {
    set_student_code(stud_while$while_cond)
    set_solution_code(sol_while$while_cond)
    eval(cond_test, envir = env)
  }
  
  # IF expression part should always be available.
  test_what(expect_false(is.null(stud_while$while_expr)), sprintf("The <code>expr</code> part%s is missing.", additionaltext))  
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
