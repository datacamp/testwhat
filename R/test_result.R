#' @export
test_result <- function(state, ...) {
  UseMethod("test_result", state)
}

#' @export
test_result.OperationState <- function(state, error_msg = NULL) {
  test_call_result(state, error_msg = error_msg, type = "operator")
}

#' @export
test_result.FunctionState <- function(state, error_msg = NULL) {
  test_call_result(state, error_msg = error_msg, type = "function")
}

#' @export
test_result.FunDefState <- function(state, ..., error_msg = NULL) {
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  run_expr_helper(state, 
                  expr = parse(text = expr_str),
                  expr_str = expr_str,
                  error_msg = error_msg,
                  case = "result")
}