#' Check whether the student printed something to the console
#'
#' Check the output of the submission to see if it contains certain elements.
#' 
#' With \code{check_output}, you can simply specify a regular expression or pattern (depending on the value of \code{fixed}) that is looked for in the student's output. 
#' 
#' With \code{test_output_contains} and \code{check_output_expr} you can pass an expression, that is executed and whose output is compared to the output the student generated. If the generated output is found in the student's output, the check passes.
#'
#' @param state the state to start from
#' @param regex the regular expression or pattern to look for
#' @param fixed if fixed is TRUE, \code{regex} will be sought for 'as is' in the output, if fixed = FALSE (the default), \code{regex} will be treated as actual regular expression.
#' @param trim should the student output be trimmed, so that all newlines and spaces are removed, before checking?
#' @param times how often should the pattern/expression output be found?
#' @param missing_msg Custom message in case the pattern or output wasn't found often enough.
#' @param ... S3 stuff
#' 
#' @param expr The expression (as string) for which the output should be in the student's console output.
#' @param incorrect_msg Custom message in case the output of the expression wasn't found often enough in the student's output.
#' 
#' @examples
#' \dontrun{
#' # Example 1
#' mtcars
#' 
#' # SCT option 1
#' test_output_contains("mtcars")
#' 
#' # SCT option 2
#' ex() %>% check_output_expr("mtcars")
#' 
#' # Example 2
#' print("hello!")
#' 
#' # SCT (robust)
#' ex() %>% check_output("[H|h]ello\\!*")
#' }
#'
#' @name test_output


#' @rdname test_output
#' @export
check_output.default <- function(state, regex, fixed = FALSE, trim = FALSE, times = 1, missing_msg = NULL, ...) {
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

#' @rdname test_output
#' @export
test_output_contains <- function(expr, times = 1, incorrect_msg = NULL) {
  ex() %>% check_output_expr(expr = expr, times = times, missing_msg = incorrect_msg)
}

#' @rdname test_output
#' @export
check_output_expr <- function(state, expr, times = 1, missing_msg = NULL) {
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
  if (length(counts) == 0) {
    return(0)
  } else {
    return(sum(counts))
  }
}

clean_up <- function(x) {
  x <- gsub("[[:space:]]|;|\n", "", x)
  x <- gsub("=", "<-", x)
  x <- gsub("FALSE", "F", x)
  x <- gsub("TRUE", "T", x)
  x <- gsub("\"", "'", x)
  return(x)
}