#' Test the student's code as text
#'
#' Some rudimentary string cleaning is performed to allow for different ways of
#' saying the same things (removing spaces, changing single quotes to double
#' quotes, changing TRUE to T ...).
#'
#' Using these function should be a last resort, as there are myriad ways of
#' solving the same problem with R!
#'
#' @param regex A set of strings/regexes that should be in the student code.
#' @param fixed if TRUE, strings are treated literally. If FALSE, strings are
#'   treated as regex patterns.
#' @param times how often should any of the strings be matched?
#' @param state the state to start from
#' @param missing_msg Custom feedback in case the pattern is not contained often
#'   enough in the student's submission.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states
#' @param drop_comments Logical value indicating whether or not to remove
#'   comments from these student code before looking for the pattern. Defaults
#'   to FALSE for backwards compatibility reasons.
#' @examples
#' \dontrun{
#' # Example 1
#' TRUE & FALSE
#'
#' # SCT
#' ex() %>% check_code(c("TRUE & FALSE", "FALSE & TRUE"), fixed = TRUE)
#'
#' # Example 2:
#' "Hello, world!"
#'
#' # SCT, robust to small typos
#' ex() %>% check_code("[H|h]ello,*\\s*[W|w]orld\\!*")
#' }
#'
#' @export
check_code <- function(state, regex,
                       fixed = FALSE, times = 1,
                       missing_msg = NULL, append = TRUE,
                       drop_comments = FALSE) {
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "typed",
                          regex = regex,
                          fixed = fixed,
                          times = times,
                          message = missing_msg,
                          append = append,
                          pd = NULL)
  student_code <- state$get("student_code")
  if (isTRUE(drop_comments)) {
    student_code <- remove_comments(student_code)
  }
  if (isTRUE(fixed)) {
    student_code <- clean_up(student_code)
    regex <- clean_up(regex)
  }
  num_hits <- get_num_hits(regex = regex, x = student_code, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = regex_state$details)
  return(state)
}

# deprecated test_student_typed
test_student_typed <- function(strings,
                               fixed = TRUE,
                               times = 1,
                               not_typed_msg = NULL) {
  fail_if_v2_only()
  ex() %>% check_code(strings, fixed = fixed, times = times, missing_msg = not_typed_msg, append = is.null(not_typed_msg))
}

