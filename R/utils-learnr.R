#' A checker function to use with learnr
#'
#' For exercise checking, learnr tutorials require a function that learnr can
#' use in the background to run the code in each "-check" chunk and to format
#' the results into a format that learnr can display. The function must accept a
#' specific set of inputs and return a specific type of output. Users are not
#' intended to use the function themselves, but to pass it to the
#' \code{exercise.checker} knitr chunk option within the setup chunk of the
#' tutorial.
#'
#' Similar to grader's \code{grade_learnr()}, testwhat provides
#' \code{testwhat_learnr()} for this purpose. To enable exercise checking in
#' your learnr tutorial, set \code{tutorial_options(exercise.checker =
#' testwhat_learnr)} in the setup chunk of your tutorial.
#'
#' @param label Label for exercise chunk
#' @param solution_code R code submitted by the user
#' @param user_code 	Code provided within the "-solution" chunk for the
#'   exercise.
#' @param check_code 	Code provided within the "-check" chunk for the exercise.
#' @param envir_result 	The R environment after the execution of the chunk.
#' @param evaluate_result The return value from the \code{evaluate::evaluate}
#'   function.
#' @param ... Unused (include for compatibility with parameters to be added in
#'   the future)
#'
#' @return An R list which contains several fields indicating the result of the
#'   check.
#' @export
testwhat_learnr <- function(label = NULL,
                            solution_code = NULL,
                            user_code = NULL,
                            check_code = NULL,
                            envir_result = NULL,
                            evaluate_result = NULL,
                            ...) {

  ######### START COPY FROM gradethis ##################
  # Sometimes no user code is provided, but
  # that means there is nothing to check. Also,
  # you do not want to parse NULL
  if (is.null(user_code)) {
    return(list(
      message = "I didn't receive your code. Did you write any?",
      correct = FALSE,
      type = "error",
      location = "append"
    ))
  }
  
  # Sometimes no solution is provided, but that
  # means there is nothing to check against. Also,
  # you do not want to parse NULL
  if (is.null(solution_code)) {
    return(list(
      message = "No solution is provided for this exercise.",
      correct = TRUE,
      type = "info",
      location = "append"
    ))
  }
  ######### END COPY FROM gradethis ##################

  setup_state(sol_code = solution_code,
              stu_code = user_code,
              sol_env = NULL,
              stu_env = envir_result,
              stu_result = evaluate_result)
  
  res <- run_until_fail(parse(text = check_code))
  return(list(message = res$message,
              correct = res$correct,
              location = "append",
              type = if(res$correct) "success" else "error"))
}
