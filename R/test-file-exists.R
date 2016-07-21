#' Test whether a file exists
#' 
#' @param path Path to the file you want to check
#' @param incorrect_msg Optional feedback message in case the file does not exist
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # write("hello", file = "test.txt")
#' 
#' # SCT to test if file exists
#' test_file_exists("test.txt")
#' }
#' 
#' @export
test_file_exists <- function(path, incorrect_msg = NULL) {
  init_tags(fun = "test_file_exists")
  
  if(is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("The file <code>%s</code> does not appear to be in your working directory; make sure you don't delete it!", path)
  }
  check_that(is_true(file.exists(path)), incorrect_msg)
}
