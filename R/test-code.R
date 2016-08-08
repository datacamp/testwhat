#' Test code for regex
#' 
#' @export
test_code <- function(state, regex, fixed = FALSE, times = 1, not_typed_msg = NULL) {
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "typed",
                          regex = regex,
                          times = times)
  res <- gregexpr(regex, text = state$get("student_code"), fixed = fixed)[[1]]
  if (any(res == -1)) {
    hits <- 0
  } else {
    hits <- length(res)
  }
  check_that(is_gte(hits, times),
             feedback = list(message = not_typed_msg,
                             details = regex_state$get("details"),
                             pd = NULL))
}
