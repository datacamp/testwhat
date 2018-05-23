#' Expectation wrapper
#'
#' This function wraps around an is_... function. When the expectation fails to
#' be met, the feedback message is sent to the reporter. You can use
#' \code{\link{is_true}}, \code{\link{is_false}}, \code{\link{is_gte}} or
#' \code{\link{is_equal}}
#'
#' @param code The expectation that should be wrapped
#' @param feedback A character string with feedback when the expection is not
#'   met OR a list object, containing multiple pieces of information. This list
#'   should at least contain an element named \code{message}
#' @param env environment in which the test should be evaluated; defaults to \code{parent.frame()}
#'
#' @examples
#' \dontrun{
#' check_that(is_true(3 == 3))
#' check_that(is_false(3 == 4))
#' check_that(is_gte(4, 3))
#' check_that(is_equal(4, 4))
#' }
#' @name check_that

#' @rdname check_that
#' @export
check_that <- function(code, feedback, env = parent.frame()) {
  
  # feedback can be a character string
  if (is.character(feedback)) {
    feedback <- list(list(message = feedback))
  }
  
  stopifnot(is.list(feedback), is.list(feedback[[1]]))
  
  res <- try(eval(code, envir = env), silent = TRUE)
  if (!isTRUE(res)) {
    throw_sct_failure(feedback = feedback,
                      message = build_feedback_message(feedback))
  }
}

throw_sct_failure <- function(message, feedback, call = sys.call(-1)) {
  sct_failed_msg <-
    c <- structure(
      class = c("sct_failure", "error", "condition"),
      list(message = message, call = call),
      feedback = feedback)
  stop(c)
}

#' @rdname check_that
#' @export
test_what <- function(code, feedback) {
  lut <- list(expect_true = call("is_true"),
              expect_false = call("is_false"),
              expect_equal = call("is_equal"))
  call <- substitute(code)
  call[1] <- lut[[as.character(call[[1]])]]
  check_that(call, feedback, env = parent.frame())
}