# #' Check whether the student output contains a pattern
# #'
# #' @param pattern The pattern to check for in the console output.
# #' @param console_output The string containing the output printed to the student's console.
# #' @param incorrect_msg feeback message in case the pattern was not found in the console output
# #' 
# #' @export
# test_output_regex <- function(pattern, console_output = get_student_output(), incorrect_msg = NULL) {
#   if(is.null(incorrect_msg)) {
#     incorrect_msg <- "The output you generated doesn't contain the pattern we're looking for."
#   }
#   collapsed_output <- paste(console_output, collapse = "\n")
#   test_that("blabla", {
#     regexpr(pattern, text = collapsed_output)
#     x <- regexpr(pattern, text = collapsed_output)[1]
#     expect_false(equals(x, -1), failure_msg = incorrect_msg)
#   })
# }