#' Check whether a file exists
#' 
#' @param path Path to the file you want to check
#' @param incorrect_msg Custom feedback message in case the file does not exist
#' @param missing_msg Custom feedback message in case the file is missing
#' @param state the state to start from
#' 
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # write("hello", file = "test.txt")
#' 
#' # SCT
#' ex() %>% check_wd("test.txt")
#' }
#' @name test_wd

#' @rdname test_wd
#' @export
check_wd <- function(state, path, missing_msg = NULL) {
  file_state <- FileState$new(state)
  file_state$add_details(type = 'file',
                         case = 'available',
                         file = basename(path),
                         folder = dirname(path),
                         message = missing_msg)
  check_that(is_true(file.exists(path)), feedback = file_state$details)
}

test_file_exists <- function(path, incorrect_msg = NULL) {
  ex() %>% check_wd(path = path, missing_msg = incorrect_msg)
}
