#' Expectation wrapper
#'
#' This function wraps around an expect_... function. When the expectation fails to be
#' met, the feedback message is sent to the reporter.
#'
#' @param code The expectation that should be wrapped
#' @param feedback_msg The feedback message that results when the expectation is not met
#' 
#' @import testthat
#' @export
test_what <- function(code, feedback_msg) {
  rep <- get_reporter()
#   if (!rep$continue && !rep$silent) {
#     stop(sct_failed_msg)
#   }
  rep$set_inh_failure_msg(feedback_msg)
  # testthat::test_that(paste0("Testing with feedback message ",feedback_msg), code)
  eval(code)
  rep$clear_inh_failure_msg()
  
  if (rep$failed && !((rep$report == "challenge") || rep$silent)) {
    stop(sct_failed_msg)
  }
  return(!rep$failed)
}

sct_failed_msg <- "<sct_failed_error>"