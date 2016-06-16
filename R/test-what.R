#' Expectation wrapper
#'
#' This function wraps around an expect_... function. When the expectation fails to be
#' met, the feedback message is sent to the reporter.
#'
#' @param code The expectation that should be wrapped
#' @param feedback A character string with feedback when the expection is not met 
#' OR a list object, containing multiple pieces of information. This list should at
#' least contain an element named \code{message}
#' 
#' @import testthat
#' @export
test_what <- function(code, feedback) {
  
  # feedback can be a character string
  if (is.character(feedback)) {
    feedback <- list(message = feedback)
  } 
  
  if (!is.list(feedback) || 
      !("message" %in% names(feedback)) || 
      is.null(feedback$message) || 
      !is.character(feedback$message) || 
      nchar(feedback$message) == 0) {
    stop("The feedback you specified in test_what() isn't in the correct format")
  }

  feedback <- c(feedback, list(tags = tw$get("tags")))
  ok <- testthat:::test_code(feedback, substitute(code), env = parent.frame())
  
  if (!ok) {
    stop(sct_failed_msg)
  }
}
