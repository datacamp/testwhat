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
  
  if(is.null(incorrect_number_of_calls_msg)) {
    incorrect_number_of_calls_msg <- sprintf("The system wants to test if the %s command you entered is correct, but it hasn't found one. Add more code.", get_num(index))
  }
  
  n_student_calls <- length(stud)
  less_or_equal <- test_what(expect_less_than(index-1, n_student_calls), incorrect_number_of_calls_msg)
  
  return(less_or_equal)
}