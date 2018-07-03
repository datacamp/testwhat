#' S3 definitions
#'
#' @param state State to start from
#' @param ... Additional arguments passed to implementations of the S3 method
#'
#' @name s3defs

#' @rdname s3defs
#' @export
check_equal <- function(state, ...) {
  UseMethod("check_equal", state)
}

#' @rdname s3defs
#' @export
check_equal.default <- function(state, ...) {
  stop("Can't run check_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

#' @rdname s3defs
#' @export
check_result <- function(state, ...) {
  UseMethod("check_result", state)
}

#' @rdname s3defs
#' @export
check_result.default <- function(state, ...) {
  stop("Can't run check_result() with a ", class(state)[1], " as input state.", call. = FALSE)
}

#' @rdname s3defs
#' @export
check_body <- function(state, ...) {
  UseMethod("check_body", state) 
}

#' @rdname s3defs
#' @export
check_body.default <- function(state, ...) {
  stop("Can't run check_body() with a ", class(state)[1], " as input state.", call. = FALSE)
}

#' @rdname s3defs
#' @export
check_option <- function(state, ...) {
  UseMethod("check_option", state) 
}

#' @rdname s3defs
#' @export
check_option.default <- function(state, ...) {
  stop("Can't run check_option() with a ", class(state)[1], " as input state.", call. = FALSE)  
}
