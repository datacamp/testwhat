#' Test code for regex
#' 
#' @export
test_code <- function(state, regex, fixed = FALSE, times = 1, missing_msg = NULL) {
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "typed",
                          regex = regex,
                          fixed = fixed,
                          times = times,
                          message = missing_msg,
                          pd = NULL)
  student_code <- state$get("student_code")
  if (fixed) {
    student_code <- clean_up(student_code)
    regex <- clean_up(regex)
  }
  num_hits <- get_num_hits(regex = regex, x = student_code, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = regex_state$details)
}


#' Test output for regex
#' @export
test_output.default <- function(state, regex, fixed = FALSE, trim = FALSE, times = 1, missing_msg = NULL) {
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "output",
                          case = "regex",
                          regex = regex,
                          times = times,
                          message = missing_msg,
                          pd = NULL)
  console_output <- convert_output_list(state$get("output_list"))
  if (trim) {
    console_output <- trim_output(console_output)
  }
  num_hits <- get_num_hits(regex = regex, x = console_output, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = regex_state$details)
  return(regex_state)
}

#' Test output for expr
#' @export
test_output_expr <- function(state, expr, times = 1, missing_msg = NULL) {
  expr_output <- tryCatch(capture.output(base::eval(parse(text = expr), envir = ex()$get("student_env"))), 
                          error = function(e) e$message)
  
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "output",
                          case = "expr",
                          expr = expr,
                          times = times,
                          message = missing_msg,
                          pd = NULL)
  
  console_output <- trim_output(convert_output_list(state$get("output_list")))
  num_hits <- get_num_hits(regex = trim_output(expr_output), x = console_output, fixed = TRUE)
  check_that(is_gte(num_hits, times), feedback = regex_state$details)
  return(regex_state)
}


## OLD FUNCTIONS

#' Test student's submission as text
#' 
#' Test whether a student typed something in his submission.
#' Some basic string formatting is performed to allow for different 
#' ways of saying the same things (removing spaces, changing single 
#' quotes to double quotes, changing TRUE to T ...).
#' 
#' Using this function should be a last resort, as there are myriad ways of
#' solving the same problems in R!
#'
#' @param strings A set of strings that should be available in the student code.
#' @param fixed if TRUE, strings are treated literally. If FALSE, strings are treated as regex patterns.
#' @param times how often should any of the strings be matched?
#' @param not_typed_msg Feedback message in case the student did not type the string.
#' 
#' @examples
#' \dontrun{
#' # Example solution code: TRUE & FALSE
#' 
#' # SCT to test this as a string (both T & F and F & T should be accepted)
#' test_student_typed(c("TRUE & FALSE", "FALSE & TRUE"))
#' }
#' 
#' @export
test_student_typed <- function(strings,
                               fixed = TRUE,
                               times = 1,
                               not_typed_msg = NULL) {
  ex() %>% test_code(strings, fixed = fixed, times = times, missing_msg = not_typed_msg)
}

#' Check whether the student printed something to the console
#'
#' Function checks whether the student's console contains the output one gets by
#' evaluating the character string provided in \code{expr} provided to expr. 
#' This function needs refactoring, as all new lines etc are removed.
#'
#' @param expr The expression (as string) for which the output should be in the student's console output.
#' @param times How often the expression's output should occur in the student's console
#' @param incorrect_msg feeback message in case the output did not contain the expression
#' 
#' @examples
#' \dontrun{
#' # SCT to test whether student printed numbers 1 to 10
#' test_output_contains("for(i in 1:10) print(i)")
#' }
#'
#' @export
test_output_contains <- function(expr, times = 1, incorrect_msg = NULL) {
  ex() %>% test_output_expr(expr = expr, times = times, missing_msg = incorrect_msg)
}

# Helper functions

convert_output_list <- function(x) {
  output_indices <- which(sapply(x, `[[`, "type") %in% c("output", "r-message", "r-warning", "r-error"))
  paste(sapply(x[output_indices], `[[`, "payload"), collapse = "\n")
}

trim_output <- function(x) {
  gsub("\n|[[:space:]]", "", x)
}

get_num_hits <- function(regex, x, fixed) {
  counts <- sapply(regex, function(patt) {
    res <- gregexpr(patt, text = x, fixed = fixed)[[1]]
    if (any(res == -1)) {
      return(0L)
    } else {
      return(length(res))
    }
  }, USE.NAMES = FALSE)
  return(sum(counts))
}

clean_up <- function(x) {
  x <- gsub("[[:space:]]|;|\n", "", x)
  x <- gsub("=", "<-", x)
  x <- gsub("FALSE", "F", x)
  x <- gsub("TRUE", "T", x)
  x <- gsub("\"", "'", x)
  return(x)
}