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
  ex() %>% test_wd(path = path, missing_msg = incorrect_msg)
}

#' @export
test_wd <- function(state, path, missing_msg = NULL) {
  file_state <- FileState$new(state)
  file_state$add_details(type = 'file',
                         case = 'available',
                         file = basename(path),
                         folder = dirname(path),
                         message = missing_msg)
  check_that(is_true(file.exists(path)), feedback = file_state$details)
}