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
  
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  test_env <- tw$get("test_env")
  
  fun_usage <- tw$get("fun_usage")
  init_tags(fun = "test_if_else")
  
  if_cond_test <- substitute(if_cond_test)
  if_expr_test <- substitute(if_expr_test)
  else_expr_test <- substitute(else_expr_test)
  
  student_structs <- extract_if(student_pd)
  solution_structs <- extract_if(solution_pd)
  
  if(length(solution_structs) < index) {
    stop(sprintf("The solution doesn't contain %s control constructs itself.", index))
  }
  
  sol_str <- solution_structs[[index]]
  
  if (!is.null(else_expr_test) && is.null(sol_str[["else_part"]])) {
      stop(sprintf("The %s control construct in the solution doesn't contain an else part itself.", get_ord(index)))
  }
  
  if(is.null(not_found_msg)) {
    not_found_msg <- sprintf(paste("The system wants to test if the %s control construct",
                                  "you coded is correct, but it hasn't found it. Add more code."), 
                            get_ord(index))
  }
  check_that(is_true(length(student_structs) >= index), feedback = list(message = not_found_msg))
  
  stud_str <- student_structs[[index]]
  additionaltext <- sprintf(" in the %s control construct of your submission", get_ord(index))

  on.exit({
    tw$set(student_pd = student_pd)
    tw$set(solution_pd = solution_pd)
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
    tw$set(fun_usage = fun_usage)
  })
  
  # IF condition part should always be there
  if (!is.null(if_cond_test)) {
    prepare_tw(stud_str, sol_str, "cond_part")
    eval(if_cond_test, envir = test_env)
  }
      
  # IF expression part should always be available.
  if (!is.null(if_expr_test)) {
    prepare_tw(stud_str, sol_str, "if_part")
    eval(if_expr_test, envir = test_env)
  }
      
  # ELSE expression part is not always be available.
  if (!is.null(else_expr_test)) {
    if (is.null(missing_else_msg)) {
      missing_else_msg = sprintf("The <code>else</code> part%s is missing.", additionaltext)
    }
    check_that(is_false(is.null(stud_str[["else_part"]])), missing_else_msg)
    prepare_tw(stud_str, sol_str, "else_part")
    eval(else_expr_test, envir = test_env)
  }
}

#' @rdname test_if_else
#' @export
test_if_else_statement <- test_if_else

