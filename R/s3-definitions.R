#' S3 definitions
#' 
#' Functions that are extended to fit different cases.
#' 
#' @param state state to start from
#' @param ... arguments passed to S3 implementations
#' 
#' @name s3definitions


#' @rdname s3definitions
#' @export
check_equal <- function(state, ...) {
  UseMethod("check_equal", state)
}

#' @rdname s3definitions
#' @export
check_equal.default <- function(state, ...) {
  stop("Can't run check_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

#' @rdname s3definitions
#' @export
check_result <- function(state, ...) {
  UseMethod("check_result", state)
}

#' @rdname s3definitions
#' @export
check_output <- function(state, ...) {
  UseMethod("check_output", state)
}

#' @rdname s3definitions
#' @export
check_error <- function(state, ...) {
  UseMethod("check_error", state)
}

#' @rdname s3definitions
#' @export
check_body <- function(state, ...) {
  UseMethod("check_body", state) 
}

#' @rdname s3definitions
#' @export
check_body.default <- function(state, ...) {
  stop("Can't run check_body() with a ", class(state)[1], " as input state.", call. = FALSE)
}

#' @rdname s3definitions
#' @export
check_file <- function(state, ...) {
    UseMethod("check_file", state)
}

#' @rdname s3definitions
#' @export
check_file.default <- function(state, ...) {
  stop("Can't run check_file() with a ", class(state)[1], " as input state.", call. = FALSE)
}




