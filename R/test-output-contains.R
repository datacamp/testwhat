#' Check whether the student printed something to the console
#'
#' Function checks whether the student's console contains the output one gets by
#' evaluating the character provided to expr. It returns TRUE when the student's
#' console indeed contains the expected output and FALSE otherwise. This function is
#' a testwhat version from an earlier datacampSCT function. (copied into this package to reduce dependency)
#'
#' This test is implemented using \code{\link{test_that}}.
#'
#' @param expr The expression (as string) for which the output should be in the student's console.
#' @param console_output The string containing the output printed to the student's console.
#' The default is DM.console.output which is set on the DataCamp server (automagically).
#' This means you don't have to specify this argument when writing SCTs for DataCamp.com.
#' @param incorrect_msg feeback message in case the output did not contain the expression
#'
#' @examples
#' \dontrun{
#' # Suppose the student has to write a loop that prints the numbers 1 up to 10.
#' # Add the following line to the SCT:
#' test_output_contains("for(i in 1:10) print(i)")
#' }
#'
#' @export
test_output_contains <- function(expr, times = 1, console_output = get_student_output(), incorrect_msg = NULL) {

  test_that(sprintf("The student printed %s to the console", expr), {
    # in reality incorrect_msg should be defined at all times... no good feedback messages result from this.
    if(is.null(incorrect_msg)) {
      incorrect_msg = sprintf("Make sure to print %s to the console",expr)
    }
    expect_that(output_contains(expr,console_output = console_output) >= times, is_true(), failure_msg = incorrect_msg)
  })
}

output_contains = function(expr, console_output = get_student_output()) {
  correct_output = try(capture.output(try(eval(parse(text=expr)))))

  if (inherits(correct_output, "try-error")) {
    return(FALSE)
  }
  correct_output = paste(correct_output, collapse='')

  # Remove new-lines:
  console_output = gsub("\n|[[:space:]]","", console_output)
  correct_output = gsub("\n|[[:space:]]","", correct_output)

  where.is.regex = gregexpr(pattern = correct_output, text = console_output, fixed = TRUE)
  if (any(where.is.regex[[1]] == (-1))) {
    return(0L)
  } else {
    return(length(where.is.regex[[1]]))
  }
}
