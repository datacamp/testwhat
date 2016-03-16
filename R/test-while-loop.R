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
#' 
#' @examples
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
                            not_found_msg = NULL,
                            env = parent.frame()) {
  
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  fun_usage <- tw$get("fun_usage")
  init_tags(fun = "test_while_loop")
  
  cond_test <- substitute(cond_test)
  if (is.character(cond_test)) code <- parse(text = cond_test)
  
  expr_test <- substitute(expr_test)
  if (is.character(expr_test)) expr_test <- parse(text = expr_test)
  
  student_whiles <- extract_while(student_pd)
  solution_whiles <- extract_while(solution_pd)
  
  if(is.null(not_found_msg)) {
    not_found_msg <- sprintf(paste("The system wants to test if the %s <code>while</code> loop",
                                   "you coded is correct, but it hasn't found it. Add more code."), 
                             get_num(index))
  }
  test_what(expect_true(length(student_whiles) >= index), feedback = list(message = not_found_msg))
  
  student_while <- student_whiles[[index]]
  solution_while <- solution_whiles[[index]]
  additionaltext <- sprintf(" in the %s <code>while</code> loop of your submission", get_num(index))
  
  on.exit({
    tw$set(student_pd = student_pd)
    tw$set(solution_pd = solution_pd)
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
    tw$set(fun_usage = fun_usage)
  })
  
  # WHILE condition part should always be there
  if(!is.null(cond_test)) {
    prepare_tw(student_while, solution_while, "cond_part")
    eval(cond_test, envir = env)
  }
  
  # IF expression part should always be available.
  if(!is.null(expr_test)) {
    prepare_tw(student_while, solution_while, "expr_part")
    eval(expr_test, envir = env)
  }
}
