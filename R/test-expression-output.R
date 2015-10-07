#' Test output of expression
#' 
#' This test tests whether the given expression gives the same output in 
#' the student and the solution environment.
#' 
#' @param expr The expression that is executed in both environments
#' @param student_env Environment in which the student's code was evaluated.
#' @param solution_env Environment in which the sample solution code was
#' evaluated.
#' @param incorrect_msg Feedback message in case the evaluation is not the
#' same in both environments. If none is given, a standard message will be
#' generated.
#' 
#' @export 
test_expression_output <- function(expr, 
                                   student_env = .GlobalEnv,
                                   solution_env = get_solution_env(),
                                   incorrect_msg = NULL) {
  
  output_sol <- try(capture.output(try(eval(parse(text = expr), envir = solution_env), silent = TRUE)))
  
  if (inherits(output_sol, "try-error")) {
    stop("expr in test_output() results in an error in the solution environment")
  }
  
  output_sol <- paste0(output_sol, collapse = "")
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Make sure that running <code>%s</code> outputs:<br><code>%s</code>.", expr, paste(output_sol, collapse = "<br>"))
  }
  
  output_stud <- try(capture.output(try(eval(parse(text = expr), envir = student_env), silent = TRUE)))
  
  if (inherits(output_stud, "try-error")) {
    test_what(fail(), 
              sprintf("%s<br>Instead, it resulted in the following error: <br><i>%s</i>", 
                      incorrect_msg, 
                      attr(output_stud,"condition")$message))
  } else {
    output_stud <- paste0(output_stud, collapse = "")
    test_what(expect_equal(output_sol, output_stud),
              sprintf("%s<br>Instead, got: <code>%s</code>", 
                      incorrect_msg, 
                      paste(output_stud, collapse = "<br>")))
  }
}