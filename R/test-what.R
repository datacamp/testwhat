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

  ok <- testthat:::test_code('my_test', substitute(code), env = parent.frame())
  if (!ok) {
    get_reporter()$set_feedback(c(feedback, list(tags = tw$get("tags"))))
    stop(sct_failed_msg)
  }
}

# run_expectation <- function(code, env = test_env()) {
#   ok <- TRUE
#   handle_error <- function(e) {
#     ok <<- FALSE
#   }
#   handle_expectation <- function(e) {
#     if(gsub("^expectation_", "", class(e)[[1]]) == "failure") {
#       ok <<- FALSE
#     }
#   }
#   handle_warning <- function(e) {
#     invokeRestart("muffleWarning")
#   }
#   handle_message <- function(e) {
#     invokeRestart("muffleMessage")
#   }
#
#   test_env <- new.env(parent = env)
#   tryCatch(
#     withCallingHandlers(
#       eval(code, test_env),
#       expectation = handle_expectation,
#       warning =     handle_warning,
#       message =     handle_message,
#       error =       handle_error
#     ),
#     # some errors may need handling here, e.g., stack overflow
#     error = handle_error
#   )
#
#   invisible(ok)
# }