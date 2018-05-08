check_equal <- function(state, ...) {
  UseMethod("check_equal", state)
}

check_equal.default <- function(state, ...) {
  stop("Can't run check_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

check_result <- function(state, ...) {
  UseMethod("check_result", state)
}

check_output <- function(state, ...) {
  UseMethod("check_output", state)
}

check_error <- function(state, ...) {
  UseMethod("check_error", state)
}

check_body <- function(state, ...) {
  UseMethod("check_body", state) 
}

check_body.default <- function(state, ...) {
  stop("Can't run check_body() with a ", class(state)[1], " as input state.", call. = FALSE)
}


