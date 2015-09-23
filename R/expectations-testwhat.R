#' Expectation: is an object defined?
#'
#' Tests whether or not an object is defined in a certain environment.
#'
#' @param env  environment in which to look for the object.
#' @seealso \code{\link{exists}}
#' @family expectations
#' @examples
#' a <- matrix(1:10, nrow = 5)
#' expect_that("a", is_defined())
#' expect_defined("a")
#' @export
is_defined <- function(env = .GlobalEnv) {
  function(name) {
    ok <- exists(name, envir = env, inherits = FALSE)
    expectation(ok, "is not defined", "is defined")
  }
}

#' @rdname is_defined
#' @export
#' @param object  name of the object to test
#' @param label  object label used in the default feedback message.  If
#' \code{NULL}, the object name is used.  This is ignored for feedback
#' messages supplied via \code{failure_msg} or \code{success_msg}.
expect_defined <- function(object, env = .GlobalEnv, info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- object
  }
  expect_that(object, is_defined(env = env), info = info, label = object)
}