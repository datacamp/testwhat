#' Test output of expression
#' 
#' Test whether the given expression gives the same output in 
#' the student and the solution environment.
#' 
#' @param expr The expression that is executed in both environments.
#' @param incorrect_msg Optional feedback message in case the evaluation is not the
#' same in both environments. Automatically generated if not specified.
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # my_fun <- function(a, b) { a + b }
#' 
#' # Test whether my_fun(1,2) and my_fun(1,2)
#' # give same _output_
#' test_function_definition({
#'  test_expression_output(my_fun(1,2))
#'  test_expression_output(my_fun(-1,-2))
#' })
#' }
#' 
#' @export 
test_expression_output <- function(expr, incorrect_msg = NULL) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  init_tags(fun = "test_expression_output")
  
  output_sol <- try(capture.output(eval(parse(text = expr), envir = solution_env)), silent = TRUE)
  
  if (length(output_sol) == 0) {
    output_sol <- NULL
  }
  
  if (inherits(output_sol, "try-error")) {
    stop("expr in test_output() results in an error in the solution environment")
  }
  
  if(!is.null(output_sol)) {
    output_sol <- paste0(output_sol, collapse = "<br>")
  }
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Make sure that running <code>%s</code> outputs%s", expr, ifelse(is.null(output_sol), " nothing", sprintf(":<br><code>%s</code>", build_summary(output_sol, output = TRUE))))
  }
  
  output_stud <- try(capture.output(eval(parse(text = expr), envir = student_env)), silent = TRUE)
  
  if (length(output_stud) == 0) {
    output_stud <- NULL
  }
  
  if (inherits(output_stud, "try-error")) {
    test_what(fail(), 
              sprintf("%s<br>Instead, it resulted in the following error:<br><i>%s</i>", 
                      incorrect_msg, 
                      build_summary(attr(output_stud,"condition")$message, output = TRUE)))
  } else {
    if(!is.null(output_stud)) {
      output_stud <- paste0(output_stud, collapse = "<br>")
    }
    test_what(expect_equal(output_sol, output_stud),
              sprintf("%s<br>Instead, got%s", 
                      incorrect_msg, 
                      ifelse(is.null(output_stud), " no output", sprintf(":<br><code>%s</code>",build_summary(output_stud, output = TRUE)))))
  }
}