#' Define the success message
#' 
#' If all tests in an SCT pass, the students gets a 
#' congratulatory message. You can specify this message with
#' /code{success_msg()}. It does not matter where in the SCT
#' you specify this message, but at the end makes most sense.
#' 
#' For multiple choice exercises, the success message is specified
#' inside \code{\link{test_mc}}, so an additional call of \code{success_msg}
#' is not necessary.
#' 
#' @param msg The congratulatory message as a character string.
#' @export
success_msg <- function(msg) {
  get_reporter()$set_inh_success_msg(msg)
}