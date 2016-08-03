#' Test a \code{while} or \code{for} loop
#' 
#' Test whether a student correctly coded a while or \code{for} loop. The function parses
#' the student and solution code and selects a loop you specify based on the\code{index} argument, 
#' and then runs two chunks of tests:
#' \itemize{
#'  \item{\code{cond_test}: \code{testwhat} tests specifically for
#'  the condition part of the while or \code{for} loop.}
#'  \item{\code{expr_test}: \code{testwhat} tests for the body of the \code{for} or \code{while} loop}
#' }
#' The tests for the conditional part and the expression part
#' of the while loop can only be text-based. You cannot appropriately use functions such
#' as \code{\link{test_object}} that also depend on the student and solution
#' environment.
#' 
#' @param index  The index of the \code{while} or \code{for} loop to check (counting for for and while loops is separate)
#' @param cond_test  SCT to perform on the condition part
#' @param expr_test  SCT to perform on the expression part
#' @param not_found_msg  Message in case the loop (at the specified index) is not found.
#' 
#' @examples
#' \dontrun{
#' # while loop - example solution code:
#' while(x < 18) {
#'  x <- x + 5
#'  print(x)
#' }
#' 
#' # SCT to test this loop:
#' test_while_loop(1, cond_test = {
#'  test_student_typed(c("< 18", "18 >"))
#' }, expr_test = {
#'  test_student_typed(c("x + 5", "5 = x"))
#'  test_function("print", eval = FALSE) # no actual value matching possible!!
#' })
#' 
#' # for loop - example solution code:
#' for(i in 1:5) {
#'  print("hurray!") 
#' }
#' 
#' # SCT to test this loop:
#' test_for_loop(1, cond_test = {
#'  test_student_typed("in")
#'  test_student_typed("1")
#'  test_student_typed("5")
#' }, expr+test = {
#'  test_function("print")
#' })
#' }
#' @name test_loop
NULL

#' @rdname test_loop
#' @export
test_while_loop <- function(index = 1, 
                            cond_test = NULL, 
                            expr_test = NULL,                          
                            not_found_msg = NULL) {
  cond_test <- substitute(cond_test)
  expr_test <- substitute(expr_test)
  test_loop(type = "while", index = index, cond_test = cond_test, expr_test = expr_test, not_found_msg = not_found_msg)
}

#' @rdname test_loop
#' @export
test_for_loop <- function(index = 1, 
                          cond_test = NULL,
                          expr_test = NULL,
                          not_found_msg = NULL) {
  cond_test <- substitute(cond_test)
  expr_test <- substitute(expr_test)
  test_loop(type = "for", index = index, cond_test = cond_test, expr_test = expr_test, not_found_msg = not_found_msg)
}


test_loop <- function(type = c("while", "for"), index, cond_test, expr_test, not_found_msg, env) {
  type <- match.arg(type)
  
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  test_env <- tw$get("test_env")
  fun_usage <- tw$get("fun_usage")
  init_tags(fun = sprintf("test_%s_loop", type))
  
  if (type == "for") {
    extract_fun <- extract_for
  } else {
    extract_fun <- extract_while
  }
  student_structs <- extract_fun(student_pd)
  solution_structs <- extract_fun(solution_pd)
  
  if (length(solution_structs) < index) {
    stop(sprintf("The solution doesn't contain %s (internal) %s loops itself.", index, type))
  }
  
  if (is.null(not_found_msg)) {
    not_found_msg <- sprintf(paste("The system wants to test if the %s `%s` loop",
                                   "you coded is correct, but it hasn't found it. Add more code."), 
                             get_ord(index), type)
  }
  check_that(is_true(length(student_structs) >= index), feedback = list(message = not_found_msg))
  
  student_struct <- student_structs[[index]]
  solution_struct <- solution_structs[[index]]
  
  on.exit({
    tw$set(student_pd = student_pd)
    tw$set(solution_pd = solution_pd)
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
    tw$set(fun_usage = fun_usage)
  })
  
  # WHILE condition part should always be there
  if (!is.null(cond_test)) {
    prepare_tw(student_struct, solution_struct, "cond_part")
    eval(cond_test, envir = test_env)
  }
  
  # IF expression part should always be available.
  if (!is.null(expr_test)) {
    prepare_tw(student_struct, solution_struct, "expr_part")
    eval(expr_test, envir = test_env)
  }
}
