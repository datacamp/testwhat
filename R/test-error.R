#' Check whether the output of the student code contained an error.
#'
#' Check whether the output of the student code contained an error, based on the DM.contains.error variable.
#' This function is implemented using \code{\link{test_that}}.
#'
#' @param contains_error boolean if the output contained an error.
#' @param incorrect_msg feeback message in case the output contained an error
#'
#' @examples
#' \dontrun{
#' # add in SCT:
#' test_error()
#' }
#'
#' @export
test_error <- function(contains_error = get_student_error(), incorrect_msg = NULL) {
  test_that("There were no errors in the code", {
    if(is.null(incorrect_msg)) {
      incorrect_msg = "Your solution contains one or more errors. Review your code."
    }
    expect_that(contains_error, is_false(), failure_msg = incorrect_msg)
  })
}
