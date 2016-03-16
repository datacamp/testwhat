#' Expectation wrapper
#'
#' This function wraps around an expect_... function. When the expectation fails to be
#' met, the feedback message is sent to the reporter.
#'
#' @param code The expectation that should be wrapped
#' @param feedback A character string with feedback when the expection is not met 
#' OR a list object, containing multiple pieces of information.
#' @param feedback_msg deprecated argument, for backwards compatibility
#' 
#' @import testthat
#' @export
test_what <- function(code, feedback, feedback_msg) {
  
  # ensure backwards compatibility
  if (!missing(feedback_msg) && missing(feedback)) {
    feedback <- list(message = feedback_msg)
  }

  # feedback can be a character string.
  if(is.character(feedback)) {
    if(nchar(feedback) == 0) {
      stop("No feedback message defined. Use test_what() around expect_ function.")
    }
    feedback <- list(message = feedback)
  }
  
  if(!is.list(feedback) || !("message" %in% names(feedback)) || is.null(feedback$message)) {
    stop(paste("the feedback object passed to test_what() isn't in the correct format;",
               "make sure it's a list that contains at least a non-NULL element named 'message'."))
  }

  # add tags
  feedback <- c(feedback, list(tags = tw$get("tags")))
  
  rep <- get_reporter()
  rep$set_data(feedback)
  eval(code)
  
  if (rep$failed) {
    stop(sct_failed_msg)
  }
}

sct_failed_msg <- "<sct_failed_error>"