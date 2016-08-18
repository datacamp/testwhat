#' Test if student coded body correctly
#' 
#' body can be of for, while or function definition
#' 
#' @param state state to start from
#' @export
test_body <- function(state, ...) {
  UseMethod("test_body", state) 
}

#' @export
test_body.default <- function(state, ...) {
  stop("Can't run test_body() with a ", class(state)[1], " as input state.", call. = FALSE)
}