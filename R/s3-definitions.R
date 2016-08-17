#' @export
test_result <- function(state, ...) {
  UseMethod("test_result", state)
}

#' @export
test_equal <- function(state, ...) {
  UseMethod("test_equal", state)
}

#' @export
test_equal.default <- function(state, ...) {
  stop("Can't run test_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

#' @export
test_output <- function(state, ...) {
  UseMethod("test_output", state)
}