#' Check whether a student used the pipe operator sufficiently (ggvis and dplyr exercises)
#'
#' Check whether a student used the pipe sufficiently. By default, the function only checks if
#' the pipe was used at least once. The user can also select the minimal
#' number of occurrences of the pipe.
#'
#' @param num minimal number of times the pipe operator has to appear (default = 1)
#' @param absent_msg feedback message in case the student did not use a single pipe.
#' @param insuf_msg feeback message in case the student did not use the pipe operator sufficiently.
#' @keywords internal
test_pipe <- function(num = 1, absent_msg = NULL, insuf_msg = NULL) {
  fail_if_v2_only()
  if (is.null(insuf_msg)) {
    insuf_msg = sprintf("You should use the pipe operator at least %i times in total in your solution code",num)
  }
  ex() %>% check_code(regex = "%>%", times = num, missing_msg = insuf_msg)
}
