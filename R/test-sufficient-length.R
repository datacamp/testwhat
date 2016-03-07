#' Test sufficient list length.
#' 
#' Is only used by additional addon testwhat packages; do not use this 
#' function inside an SCT.
#' 
#' @param stud list of student commands
#' @param index index to fetch from these commands
#' @param incorrect_number_of_calls_msg message in case the length of stud is not sufficient.
#' 
#' @export
test_sufficient_length = function(stud, index) {
  if(index < 1) {
    stop("The index argument must be positive!")
  }
  
  n_student_calls <- length(stud)
  sufficient_length <- (index <= n_student_calls)
  test_what(expect_true(sufficient_length), incorrect_number_of_calls_msg)
  
  return(sufficient_length)
}