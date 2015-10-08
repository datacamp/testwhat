#' Test whether the library function was called correctly
#' 
#' Convenience function to test in a very hacky way whether 
#' the library function was called correctly in its most simple form.
#' There is support for the different ways to call the library function
#' 
#' @param package package name for which the library()
#' function should've been called
#' @param not_called_msg optional feedback message in case the library
#' function wasn't called a single time
#' @param incorrect_msg optional feedback message in case the library
#' function wasn't called for the specified package.
#' @inheritParams test_object
#' 
#' @examples
#' \dontrun{
#' # Example solution code
#' library(ggvis)
#' library(dplyr)
#' 
#' # SCT to test both library calls:
#' test_library_function("ggvis")
#' test_library_function("dplyr")
#' }
#' 
#' @export
test_library_function <- function(package,
                          student_code = get_student_code(),
                          not_called_msg = NULL, 
                          incorrect_msg = NULL) {
  
  if(is.null(not_called_msg)) {
    not_called_msg <- sprintf("Make sure to call the <code>library()</code> function to load the <code>%s</code> package", package)
  }
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Have you correctly called the <code>library()</code> function to load the <code>%s</code> package?", package)
  }
  
  test_what(expect_true(grepl("library\\(", student_code)), not_called_msg)
  test_what(expect_true(grepl(sprintf("library\\(%s\\)|library\\(\"%s\"\\)|library\\('%s'\\)",package,package,package), student_code)), 
            incorrect_msg)
}