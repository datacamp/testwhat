#' @export
test_output <- function(state, ...) {
  UseMethod("test_output", state)
}

#' Test output for regex
#' @export
test_output.default <- function(state, regex, fixed = FALSE, trim = FALSE, times = 1, missing_msg = NULL) {
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "output",
                          case = "regex",
                          regex = regex,
                          times = times,
                          message = missing_msg,
                          pd = NULL)
  console_output <- convert_output_list(state$get("output_list"))
  if (trim) {
    console_output <- trim_output(console_output)
  }
  num_hits <- get_num_hits(regex = regex, x = console_output, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = regex_state$details)
  return(regex_state)
}


#' @export
test_output.ExprState <- function(state, error_msg = NULL) {
  expr <- state$get("expr")
  run_expr_helper(state, 
                  expr = expr,
                  expr_str = as.character(expr),
                  error_msg = error_msg,
                  case = "output")
}


#' @export
test_output.FunDefState <- function(state, ..., error_msg = NULL) {
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  run_expr_helper(state, 
                  expr = parse(text = expr_str),
                  expr_str = expr_str,
                  error_msg = error_msg,
                  case = "output")
}