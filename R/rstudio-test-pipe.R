#' Test whether a student used the pipe operator sufficiently (ggvis and dplyr exercises)
#'
#' Test whether a student used the pipe sufficiently. By default, the function only checks if
#' the pipe was used at least once. The user can also select the minimal
#' number of occurrences of the pipe.
#'
#' This test is implemented using \code{\link{test_that}}.
#'
#' @param num minimal number of times the pipe operator has to appear (default = 1)
#' @param absent_msg feedback message in case the student did not use a single pipe.
#' @param insuf_msg feeback message in case the student did not use the pipe operator sufficiently.
#'
#' @export
test_pipe <- function(num = 1, absent_msg = NULL, insuf_msg = NULL) {

  student_code <- tw$get("student_code")
  init_tags(fun = "test_pipe")

  where.is.regex = gregexpr(pattern = '%>%', text = student_code, fixed = TRUE)
  if(is.null(absent_msg)) {
    absent_msg = "You did not use the pipe operator at all. Do this and see how your code simplifies!"
  }
  test_what(expect_false(any(where.is.regex[[1]] == (-1))), feedback_msg = absent_msg)
  if(is.null(insuf_msg)) {
    insuf_msg = sprintf("You should use the pipe operator at least %i times in total in your solution code",num)
  }
  test_what(expect_true(length(where.is.regex[[1]]) >= num), feedback_msg = insuf_msg)
}
