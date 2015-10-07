#' Run all tests for an exercise
#'
#' Run all tests for an exercise and report the results (including feedback).
#'
#' Function \code{test_exercise()} allows to supply feedback messages with
#' arguments \code{failure_msg} (only if \code{report = "all"}) and
#' \code{success_msg}.  Alternatively, functions \code{failure_msg()} and
#' \code{success_msg()} can be called within the test code to set the
#' corresponding feedback messages.
#'
#' See vignette \code{"testwhat-intro"} for examples.
#'
#' @param code  character string containing the tests to perform.
#' @param report  specifies how to report feedback in case of failed tests.  If
#' \code{"first"}, only the feedback message from the first failed test is
#' reported.  If \code{"all"}, feedback messages from all failed tests are
#' collected.
#' @param env  environment in which to execute tests.
#' @param failure_msg  if \code{report = "all"}, text to be included before the
#' collected feedback messages in case of failed tests.  Otherwise ignored.
#' @param success_msg  feedback message in case all tests are successful.
#' @param msg  see \dQuote{Details}.
#'
#' @return A list with components \code{passed} that indicates whether all
#' tests were sucessful, and \code{feedback} that contains a feedback message.
#'
#' @export
test_exercise <- function(code, report = c("first", "all", "challenge"), failure_msg = NULL,
                          success_msg = NULL, env = test_env()) {
  # Parse code and ensure that feedback messages are reset
  code <- parse(text = code)
  report <- match.arg(report)
  feedback_msg <- options(failure_msg = NULL, success_msg = NULL)
  on.exit(options(feedback_msg))

  # Execute code with the DataCamp reporter such that it collects test results
  reporter <- DataCampReporter$new(report=report)
  with_reporter(reporter, .test_exercise(code, env))

  # Obtain feedback from DataCamp reporter and return it invidibly
  failure_msg <- getOption("failure_msg", default = failure_msg)
  success_msg <- getOption("success_msg", default = success_msg)
  feedback <- reporter$get_feedback(failure_msg, success_msg)
  invisible(feedback)
}

.test_exercise <- function(code, parent_env) {
  n <- length(code)
  if (n == 0L) return(invisible())
  # Try because if sct fails, execution is thrown back here.
  try(eval(code, new.env(parent = parent_env)))
  end_context()
}

end_context <- function() {
  rep <- get_reporter()
  if (!rep$context_open) return(invisible())
  rep$end_context()
  rep$context_open <- FALSE
  invisible()
}

#' @rdname test_exercise
#' @export
failure_msg <- function(msg) options(failure_msg = msg)

#' @rdname test_exercise
#' @export
success_msg <- function(msg) options(success_msg = msg)
