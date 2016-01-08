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
#' @param strings A set of strings, at least one of which must be in the student_code
#' @param fixed exact string matching (TRUE) or use regex (FALSE)?
#' @param not_typed_msg Feedback message in case the student did not type the string.
#' @inheritParams test_function
#' 
#' @examples
#' \dontrun{
#' # Example solution code: TRUE & FALSE
#' 
#' # SCT to test this as a string (both T & F and F & T should be accepted)
#' test_student_typed(c("TRUE & FALSE", "FALSE & TRUE"))
#' }
#' 
#' @import datacampAPI
#' @import testthat
#' @export
test_student_typed <- function(strings,
                               fixed = TRUE,
                               not_typed_msg = NULL) {
  
  student_code = tw$get("student_code")
  
  if(is.null(not_typed_msg)) {
    not_typed_msg <- sprintf("The solution expects you to type %s at the appropriate location%s.", 
                             collapse_args(strings, conn = " or "), if(length(strings) == 1) "" else "s")
  }
  
  # Clean up both string and student code
  student_code <- gsub("[[:space:]]|;|\n", "", student_code)
  strings <- gsub("[[:space:]]|;|\n", "", strings)
  student_code <- gsub("=", "<-", student_code)
  strings <- gsub("=", "<-", strings)
  student_code <- gsub("FALSE", "F", student_code)
  strings <- gsub("FALSE", "F", strings)
  student_code <- gsub("TRUE", "T", student_code)
  strings <- gsub("TRUE", "T", strings)
  student_code <- gsub("\"", "'", student_code)
  strings <- gsub("\"", "'", strings)
  
  hits <- sapply(strings, grepl, x = student_code, fixed = fixed)
  
  test_what(expect_true(any(hits)), not_typed_msg)
}
