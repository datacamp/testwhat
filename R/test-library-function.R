#' Test whether the library function was called correctly
#' 
#' Convenience function to test in a very hacky way whether the library function was called correctly in its most simple form.
#' There is support for the different ways to call the library function
#' 
#' @param package package name for which the library() function should've been called
#' @param student_code  character string containing the student's code.
#' @param not_called_msg Feedback message in case the library function wasn't called a single time
#' @param incorrect_msg Feedback message in case the library function wasn't called for the specified package.
test_library_function <- function(package,
                          student_code = get_student_code(),
                          not_called_msg = NULL, incorrect_msg = NULL) {
  
  if(is.null(not_called_msg)) {
    not_called_msg <- sprintf("Make sure to call the <code>library()</code> function to load the <code>%s</code> package", package)
  }
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Have you correctly called the <code>library()</code> function to load the <code>%s</code> package?", package)
  }
  
  test_that(sprintf("Library function called correctly"), {
    expect_that(grepl("library\\(", student_code), is_true(), failure_msg = not_called_msg)
    expect_that(grepl(sprintf("library\\(%s\\)|library\\(\"%s\"\\)|library\\('%s'\\)",package,package,package), student_code), 
                is_true(), failure_msg = incorrect_msg)
  })
}