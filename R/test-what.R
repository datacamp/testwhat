#' @importFrom testthat test_that
test_what <- function(code, feedback_msg) {
  rep <- get_reporter()
  if (!rep$continue) return(FALSE)
  rep$toggle_inherit_failure(TRUE, feedback_msg)
  testthat::test_that(paste0("Testing with feedback message ",feedback_msg), code)
  rep$toggle_inherit_failure()
  return(!rep$failed)
}
