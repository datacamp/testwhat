#' Test a multiple choice exercise
#'
#' Test a multiple choice exercise using \code{\link{test_what}}. 
#' This code expects the DM.result variable to be defined by the DataCamp frontend. 
#' There is need to define the success_msg seperately, since it is defined inside the function.
#'
#' @param correct number of the correct answer
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
#' @import datacampAPI
#' @import testthat
#' @export
test_mc <- function(correct = NULL, no_selection_msg = NULL, feedback_msgs = NULL) {
  
  result = get("DM.result", env = globalenv())
  
  if(is.null(correct)) {
    stop("argument 'correct' can not be empty")
  }

  #default messages:
  if(is.null(no_selection_msg)) {
    no_selection_msg <- "Please select one of the options!"
  }
  if(is.null(feedback_msgs)) {
    incorrect_msgs <- rep("Your answer is incorrect. Try again.",20) #20 hardcoded, should never be more..
    default_success_msg = "Very good! Continue to the next exercise."
    feedback_msgs = c(incorrect_msgs[1:correct-1],default_success_msg,incorrect_msgs[correct:length(incorrect_msgs)])
  }
  
  if(is.na(feedback_msgs[result])) {
    stop("There is no feedback message available for this user input! Make sure you define enough feedback messages")
  }
  
  defined <- exists("result")
  test_what(expect_true(defined), no_selection_msg)
  if(defined) {
    if(!is.numeric(result)) {
      stop("There is something wrong with the result set by the backend.")
    }
    test_what(expect_identical(correct,result), feedback_msgs[result])
  }

  success_msg(feedback_msgs[correct])
}
