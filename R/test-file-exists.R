#' Test whether a file is availabe in the working directory
#' 
#' @param path Path to the file you want to check
#' @param incorrect_msg Feedback message in case the file does not exist
#' @export
test_file_exists <- function(path, incorrect_msg = NULL) {
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("The file \"%s\" does not appear to be in your working directory; make sure you don't delete it!", path)
  }
  
  test_what(expect_true(path %in% dir()), incorrect_msg)
}
