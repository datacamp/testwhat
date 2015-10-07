#' Define the success message
#' 
#' If all tests in an SCT pass, the students gets a 
#' congratulatory message. You can specify this message with
#' /code{success_msg()}. It does not matter where in the SCT
#' you specify this message, but at the end makes most sense.
#' 
#' @param msg The congratulatory message as a character string.
#' @export
success_msg <- function(msg) options(success_msg = msg)
