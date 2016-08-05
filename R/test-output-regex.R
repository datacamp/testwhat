#' Check whether the student output contains a pattern
#'
#' @param pattern The pattern to check for in the console output.
#' @param fixed logical. if \code{TRUE}, \code{pattern} is matched to the output as is, instead of as regular expression. 
#' @param times How often the pattern should be matched
#' @param incorrect_msg feeback message in case the pattern was not found in the console output
#' 
#' @export
test_output_regex <- function(pattern, fixed = FALSE, times = 1, incorrect_msg = NULL) {
  init_tags(fun = "test_output_regex")
  console_output = get("DM.console.output", envir = tw$get("student_env"))

  output_list <- tw$get("output_list")
  output_indices <- which(sapply(output_list, `[[`, "type") %in% c("output", "r-message", "r-warning", "r-error"))
  outputs <- sapply(output_list[output_indices], `[[`, "payload")
  
  if (is.null(incorrect_msg)) {
    incorrect_msg <- "The output that your code generated doesn't contain the pattern we're looking for."
  }
  
  matches_list <- gregexpr(pattern = pattern, text = outputs, fixed = fixed)
  
  counts <- sapply(matches_list, function(matches) {
    if (isTRUE(matches == -1)) {
      return(0L)
    } else {
      return(length(matches))
    }
  }, USE.NAMES = FALSE)
  
  check_that(is_gte(sum(counts), times), feedback = list(message = incorrect_msg))
}