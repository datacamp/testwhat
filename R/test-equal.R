test_equal <- function(state, ...) UseMethod("test_equal", state)

test_equal.default <- function(state, ...) {
  stop("Can't run test_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}



