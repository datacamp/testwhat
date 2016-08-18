## OLD FUNCTIONS

#' Test student's submission as text
#' 
#' Test whether a student typed something in his submission.
#' Some basic string formatting is performed to allow for different 
#' ways of saying the same things (removing spaces, changing single 
#' quotes to double quotes, changing TRUE to T ...).
#' 
#' Using this function should be a last resort, as there are myriad ways of
#' solving the same problems in R!
#'
#' @param strings A set of strings that should be available in the student code.
#' @param fixed if TRUE, strings are treated literally. If FALSE, strings are treated as regex patterns.
#' @param times how often should any of the strings be matched?
#' @param not_typed_msg Feedback message in case the student did not type the string.
#' 
#' @examples
#' \dontrun{
#' # Example solution code: TRUE & FALSE
#' 
#' # SCT to test this as a string (both T & F and F & T should be accepted)
#' test_student_typed(c("TRUE & FALSE", "FALSE & TRUE"))
#' }
#' 
#' @export
test_student_typed <- function(strings,
                               fixed = TRUE,
                               times = 1,
                               not_typed_msg = NULL) {
  ex() %>% test_code(strings, fixed = fixed, times = times, missing_msg = not_typed_msg)
}

#' Check whether the student printed something to the console
#'
#' Function checks whether the student's console contains the output one gets by
#' evaluating the character string provided in \code{expr} provided to expr. 
#' This function needs refactoring, as all new lines etc are removed.
#'
#' @param expr The expression (as string) for which the output should be in the student's console output.
#' @param times How often the expression's output should occur in the student's console
#' @param incorrect_msg feeback message in case the output did not contain the expression
#' 
#' @examples
#' \dontrun{
#' # SCT to test whether student printed numbers 1 to 10
#' test_output_contains("for(i in 1:10) print(i)")
#' }
#'
#' @export
test_output_contains <- function(expr, times = 1, incorrect_msg = NULL) {
  ex() %>% test_output_expr(expr = expr, times = times, missing_msg = incorrect_msg)
}