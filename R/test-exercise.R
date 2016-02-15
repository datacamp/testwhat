#' Run all tests for an exercise
#'
#' Run all tests for an exercise and report the results (including feedback).
#' This function is run by R Backend and should not be used by course creators.
#'
#' @param code character string containing the tests to perform.
#' @param type the type of the exercise.
#' @param env  environment in which to execute tests.
#'
#' @return A list with components \code{passed} that indicates whether all
#' tests were sucessful, and \code{feedback} that contains a feedback message.
#'
#' @export
test_exercise <- function(code, 
                          ex_type, 
                          pec,
                          student_code,
                          solution_code,
                          solution_env,
                          output_list,
                          env = test_env()) {
  
  # Store everything that's needed locally
  tw$initialize(list(pec = pec,
                     student_code = student_code,
                     # student_pd = getParseData(parse(text = paste(get_clean_lines(student_code), collapse = "\n"), keep.source = TRUE)),
                     student_env = globalenv(),
                     solution_code = solution_code,
                     # solution_pd = getParseData(parse(text = paste(get_clean_lines(solution_code), collapse = "\n"), keep.source = TRUE)),
                     solution_env = solution_env,
                     output_list = output_list))
  
  # Parse code and ensure that feedback messages are reset
  code <- parse(text = code)
  feedback_msg <- options(failure_msg = NULL, success_msg = NULL)
  on.exit(options(feedback_msg))

  # Execute code with the DataCamp reporter such that it collects test results
  reporter <- DataCampReporter$new(ex_type=ex_type)
  with_reporter(reporter, .test_exercise(code, env))

  # Obtain feedback from DataCamp reporter and return it invisibly
  reporter$get_feedback()
}

.test_exercise <- function(code, parent_env) {
  get_reporter()$start_reporter()
  n <- length(code)
  if (n == 0L) return(invisible())
  # Try because if sct fails, execution is thrown back here.
  eval_fail <- try(eval(code, new.env(parent = parent_env)), silent = TRUE)
  if (inherits(eval_fail, "try-error")) {
    cond <- attr(eval_fail, "condition")$message
    if (!identical(cond, sct_failed_msg)) {
      stop(attr(eval_fail, "condition"))
    }
  }
}