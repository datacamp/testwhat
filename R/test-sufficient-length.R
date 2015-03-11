#' Helper function to test if list has sufficient length
#' 
#' @param stud list of student commands
#' @param index index to fetch from these commands
#' @param incorrect_number_of_calls_msg message in case the length of stud is not sufficient.
#' @export
test_sufficient_length = function(stud,index,incorrect_number_of_calls_msg = NULL) {
  if(index < 1) {
    stop("The index argument must be positive!")
  }
  n_student_calls <- length(stud)
  test_that("Student and solution have an equal number of commands", {
    # build default message
    if(is.null(incorrect_number_of_calls_msg)) {
      incorrect_number_of_calls_msg <- sprintf("The system wants to test if the %s command you entered is correct, but it hasn't found one. Add more code.", get_num(index))
    }
    expect_that(index <= n_student_calls, is_true(), failure_msg = incorrect_number_of_calls_msg)
  })
  return(index <= n_student_calls)
}