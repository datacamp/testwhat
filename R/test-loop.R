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
  test_loop(index = index, 
            cond_test = cond_test,
            expr_test = expr_test,
            not_found_msg = not_found_msg,
            fun = test_while)
}

#' @rdname test_loop
#' @export
test_for_loop <- function(index = 1, 
                          cond_test = NULL,
                          expr_test = NULL,
                          not_found_msg = NULL) {
  cond_test <- substitute(cond_test)
  expr_test <- substitute(expr_test)
  test_loop(index = index, 
            cond_test = cond_test,
            expr_test = expr_test,
            not_found_msg = not_found_msg,
            fun = test_for)
}

test_loop <- function(index, cond_test, expr_test, not_found_msg, fun) {
  old_state <- ex()
  test_env <- old_state$get("test_env")
  on.exit(tw$set(state = old_state))
  
  testloop <- old_state %>% fun(index = index, not_found_msg = not_found_msg)
  
  if (!is.null(cond_test)) {
    cond_state <- testloop %>% test_cond()
    tw$set(state = cond_state)
    eval(cond_test, envir = test_env)
  }

  if (!is.null(expr_test)) {
    body_state <- testloop %>% test_body()
    tw$set(state = body_state)
    eval(expr_test, envir = test_env)
  }
}