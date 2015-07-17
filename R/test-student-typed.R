#' Test whether a student typed something
#' 
#' Test whether a student typed something. 
#' Some basic string formatting is performed to allow for different 
#' ways of saying the same things (removing spaces, changing single 
#' quotes to double quotes, changing TRUE to T ...)
#'
#' @param strings A set of strings, at least one of which must be in the student_code
#' @param student_code student submission as a string
#' @param not_typed_msg Feedback message in case the student did not type the string.
#' @export
test_student_typed <- function(strings,
                               student_code = get_student_code(),
                               fixed = TRUE,
                               not_typed_msg = NULL) {
  
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
  
  test_that("one of strings found in student_code", {
    hits <- sapply(strings, grepl, x = student_code, fixed = fixed)
    expect_that(any(hits), is_true(), failure_msg = not_typed_msg)
  })
}
