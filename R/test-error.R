#' Check whether the output of the student code contained an error.
#'
#' Check whether the output of the student code contained an error, based on the DM.contains.error variable.
#' This function is implemented using \code{\link{test_that}}.
#'
#' @param error The error message if the output contained an error, NULL if there was no error
#' @param incorrect_msg feeback message in case the output contained an error
#'
#' @examples
#' \dontrun{
#' # add in SCT:
#' test_error()
#' }
#'
#' @export
test_error <- function(error = get_student_error(), incorrect_msg = NULL) {
  test_that("There were no errors in the code", {
    expect_that(is.null(error), is_true(),
                failure_msg = ifelse(is.null(incorrect_msg), 
                                     sprintf("Your solution contains an error:<br><i>%s</i>", error),
                                     incorrect_msg))
  })
}