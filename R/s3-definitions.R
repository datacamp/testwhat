#' S3 definitions
#' 
#' Functions that are extended to fit different cases.
#' 
#' @param state state to start from
#' @param ... arguments passed to S3 implementations
#' 
#' @name s3definitions

#' @rdname s3definitions
check_equal <- function(state, ...) {
  UseMethod("check_equal", state)
}

#' @rdname s3definitions
check_equal.default <- function(state, ...) {
  stop("Can't run check_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

#' @rdname s3definitions
check_result <- function(state, ...) {
  UseMethod("check_result", state)
}

#' @rdname s3definitions
check_output <- function(state, ...) {
  UseMethod("check_output", state)
}

#' @rdname s3definitions
check_error <- function(state, ...) {
  UseMethod("check_error", state)
}

#' @rdname s3definitions
check_body <- function(state, ...) {
  UseMethod("check_body", state) 
}

#' @rdname s3definitions
check_body.default <- function(state, ...) {
  stop("Can't run check_body() with a ", class(state)[1], " as input state.", call. = FALSE)
}





