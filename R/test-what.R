#' Expectation wrapper
#'
#' This function wraps around an expect_... function. When the expectation fails to be
#' met, the feedback message is sent to the reporter.
#'
#' @param code The expectation that should be wrapped
#' @param feedback_msg The feedback message that results when the expectation is not met
#' @importFrom testthat test_that
#' @export
test_what <- function(code, feedback_msg) {
  rep <- get_reporter()
  if (!rep$continue) stop("stop here because sct test has failed")
  rep$toggle_inherit_failure(TRUE, feedback_msg)
  testthat::test_that(paste0("Testing with feedback message ",feedback_msg), code)
  rep$toggle_inherit_failure()
  return(!rep$failed)
}

