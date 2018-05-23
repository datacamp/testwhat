#' Check if object is true, false or >=
#'
#' Utility functions to use inside \code{\link{check_that}}.
#'
#' @param x object to test
#' @param y single numeric value to compare
#' @name compare

#' @rdname compare
#' @export
is_true <- function(x) {
  identical(as.vector(x), TRUE)
}

#' @rdname compare
#' @export
is_false <- function(x) {
  identical(as.vector(x), FALSE)
}


#' @rdname compare
#' @export
is_gte <- function(x, y) {
  stopifnot(is.numeric(x), length(x) == 1)
  stopifnot(is.numeric(y), length(y) == 1)
  x >= y
}
