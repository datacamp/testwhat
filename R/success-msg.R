#' Define the success message
#' 
#' If all tests in an SCT pass, the student is presented with a 
#' congratulatory message. You can specify this message with
#' /code{success_msg()}. It does not matter where in the SCT
#' you specify this message, but at the end makes most sense.
#' 
#' For multiple choice exercises, the success message is specified
#' inside \code{\link{check_mc}}, so an additional call of \code{success_msg}
#' is not necessary.
#' 
#' @param msg The success message as a character string.
#' @param praise Whether or not to prepend a message of praise from the \code{praise} package.
#' 
#' @export
#' @importFrom praise praise
success_msg <- function(msg, praise = FALSE) {
  if (isTRUE(praise)) {
    msg <- paste(praise::praise(), msg)
  }
  tw$set(success_msg = capitalize(trim(msg)))
}
