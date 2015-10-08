#' Check whether the student printed something to the console
#'
#' Function checks whether the student's console contains the output one gets by
#' evaluating the character string provided in \code{expr} provided to expr. 
#' This function needs refactoring, as all new lines etc are removed.
#'
#' @param expr The expression (as string) for which the output should be in the student's console output.
#' @param times How often the expression's output should occur in the student's console
#' @param console_output The string containing the output printed to the student's console.
#' The default is DM.console.output which is set on the DataCamp server (automagically).
#' This means you don't have to specify this argument when writing SCTs for DataCamp.com.
#' @param incorrect_msg feeback message in case the output did not contain the expression
#'
#' @examples
#' \dontrun{
#' # SCT to test whether student printed numbers 1 to 10
#' test_output_contains("for(i in 1:10) print(i)")
#' }
#'
#' @import datacampAPI
#' @import testthat
#' @export
test_output_contains <- function(expr, times = 1, console_output = get_student_output(), incorrect_msg = NULL) {
    # in reality incorrect_msg should be defined at all times... no good feedback messages result from this.
    if(is.null(incorrect_msg)) {
      incorrect_msg <- build_incorrect_output_msg(expr)
    }
  test_what(expect_true(output_contains(expr,console_output = console_output) >= times),
            incorrect_msg)
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
