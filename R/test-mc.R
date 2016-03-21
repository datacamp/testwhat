#' Test a multiple choice exercise
#'
#' Test a multiple choice exercise using \code{\link{test_what}}. 
#' This code expects the DM.result variable to be defined by the DataCamp frontend. 
#' There is need to define the success_msg seperately, since it is defined inside the function.
#'
#' @param correct number of the correct answer (or vector of numbers, if several options are fine)
#' @param no_selection_msg feedback message in case the student did not select an answer.
#' @param feedback_msgs vector of feedback messages for both the incorrect exercises as the correct exercise.
#' Order the messages according to how they are listed in the instructions. For example, if there are four options,
#' the second of which is correct, a vector of four feedback messages should be provided. The first message corresponds
#' to feedback on the incorrect selection of the first option, the second message corresponds to the feedback message for
#' the correct collection. The third and fourth messages correspond to feedback on the incorrect selection of the third and
#' fourth option.
#' 
#' @examples
#' \dontrun{
#' # Example solution: second instruction correct.
#' 
#' # Corresponding SCT:
#' msg1 <- "Not good, try again!"
#' msg2 <- "Nice one!"
#' msg3 <- "Not quite, give it another shot."
#' msg4 <- "Don't be silly..."
#' test_mc(2, feedback_msgs = c(msg1, msg2, msg3, msg4))
#' }
#'
#' @export
test_mc <- function(correct, no_selection_msg = NULL, feedback_msgs = NULL) {
  init_tags(fun = "test_mc")
  
  # see if DM.result exists
  if(is.null(no_selection_msg)) {
    no_selection_msg <- "Please select one of the options!"
  }
  
  test_what(exists("DM.result", envir = globalenv()), no_selection_msg)
  result <- get("DM.result", envir = globalenv())
  
  # see if result is correct
  if(!is.null(feedback_msgs) && is.na(feedback_msgs[result])) {
    stop("There is no feedback message available for this user input! Make sure you define enough feedback messages.")
  }
  
  test_what(expect_true(result %in% correct), feedback_msg = ifelse(is.null(feedback_msgs), "Your answer is incorrect. Try again.", feedback_msgs[result]))

  success_msg(ifelse(is.null(feedback_msgs), "Very good! Continue to the next exercise.", feedback_msgs[correct]))
}
