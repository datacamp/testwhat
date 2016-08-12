#' Test a conditional statement
#' 
#' Check whether the student correctly coded a conditional statement. 
#' The function parses all \code{if-else} constructs and then runs tests for 
#' all composing parts of this constructions.
#' 
#' If there's an \code{else if} in there, this counts as a 'sub-conditional'
#' statement (see example).
#' 
#' @param index The index of the control structure to check.
#' @param if_cond_test tests to perform in the if condition part 
#' of the control structure
#' @param if_expr_test tests to perform in the if expression part 
#' of the control structure
#' @param else_expr_test tests to perform in the else expression 
#' part of the control structure
#' @param not_found_msg Message in case the control structure 
#' (at given index) is not found.
#' @param missing_else_msg Messing in case the else part of the 
#' control structure should be there but is missing
#' 
#' @examples
#' \dontrun{
#' # Example solution code
#' vec <- c("a", "b", "c")
#' if("a" %in% vec) {
#'  print("a in here")
#' } else if(any("b" > vec)) {
#'  cat("b not smallest")
#' } else {
#'  str(vec)
#' }
#' 
#' # SCT to test this loop
#' test_if_else({
#'  test_student_typed("%in%")
#' }, {
#'  # test if expr part
#'  test_function("print")
#' }, {
#'  # test else expr part
#'  test_if_else({
#'    # test cond part of else if
#'    test_student_typed(">")
#'  }, {
#'    # test else if expr part
#'    test_function("cat")
#'  }, {
#'    # test else part
#'    test_function("str")
#'  })
#' })
#' }
#' 
#' @export
test_if_else <- function(index = 1, 
                         if_cond_test = NULL, 
                         if_expr_test = NULL, 
                         else_expr_test = NULL,
                         not_found_msg = NULL,
                         missing_else_msg = NULL) {
  old_state <- ex()  
  test_env <- old_state$get("test_env")
  on.exit(tw$set(state = old_state))
  
  testif <- old_state %>% test_ifelse(index = index, not_found_msg = not_found_msg)
  
  if_cond_test <- substitute(if_cond_test)
  if (!is.null(if_cond_test)) {
    cond_state <- testif %>% test_cond()
    tw$set(state = cond_state)
    eval(if_cond_test, envir = test_env)
  }
  
  if_expr_test <- substitute(if_expr_test)
  if (!is.null(if_expr_test)) {
    ifexprstate <- testif %>% test_if()
    tw$set(state = ifexprstate)
    eval(if_expr_test, envir = test_env)
  }
  
  else_expr_test <- substitute(else_expr_test)
  if (!is.null(else_expr_test)) {
    elseexprstate <- testif %>% test_else(not_found_msg = missing_else_msg)
    tw$set(state = elseexprstate)
    eval(else_expr_test, envir = test_env)
  }
}

