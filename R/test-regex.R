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
  if(identical(expr_output, character(0))) {
    stop(sprintf("Running `%s` in the student environment didn't generate any output, but it should, for `test_output_expr()` to make sense.", expr))
  }
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