#' Test the student's code as text
#' 
#' Some rudimentary string cleaning is performed to allow for different ways of
#' saying the same things (removing spaces, changing single quotes to double
#' quotes, changing TRUE to T ...).
#' 
#' Using these function should be a last resort, as there are myriad ways of 
#' solving the same problems in R!
#' 
#' Watch out: in \code{test_student_typed} the default is \code{fixed = TRUE},
#' in \code{check_code} the default is \code{fixed = FALSE}.
#' 
#' @param strings A set of strings/regexes that should be in the student code.
#' @param regex A set of strings/regexes that should be in the student code.
#' @param fixed if TRUE, strings are treated literally. If FALSE, strings are
#'   treated as regex patterns.
#' @param times how often should any of the strings be matched?
#' @param not_typed_msg Custom feedback in case the pattern is not contained
#'   often enough in the student's submission.
#' @param state the state to start from
#' @param missing_msg Custom feedback in case the pattern is not contained often
#'   enough in the student's submission.
#' @param append Whether or not to append the feedback to feedback built in previous states
#' 
#' @examples
#' \dontrun{
#' # Example 1
#' TRUE & FALSE
#' 
#' # SCT option 1
#' test_student_typed(c("TRUE & FALSE", "FALSE & TRUE"))
#' 
#' # SCT option 2
#' ex() %>% check_code(c("TRUE & FALSE", "FALSE & TRUE"), fixed = TRUE)
#' 
#' # Example 2:
#' "Hello, world!"
#' 
#' # SCT option 1, robust to small typos
#' test_student_typed("[H|h]ello,*\\s*[W|w]orld\\!*", fixed = FALSE)
#' 
#' # SCT option 2, robust to small typos
#' ex() %>% check_code("[H|h]ello,*\\s*[W|w]orld\\!*")
#' }
#' 
#' @name test_code

#' @rdname test_code
#' @export
test_student_typed <- function(strings,
                               fixed = TRUE,
                               times = 1,
                               not_typed_msg = NULL) {
  ex() %>% check_code(strings, fixed = fixed, times = times, missing_msg = not_typed_msg, append = is.null(not_typed_msg))
}

#' @rdname test_code
#' @export
check_code <- function(state, regex, fixed = FALSE, times = 1, missing_msg = NULL, append = TRUE) {
  regex_state <- RegexState$new(state)
  regex_state$add_details(type = "typed",
                          regex = regex,
                          fixed = fixed,
                          times = times,
                          message = missing_msg,
                          append = append,
                          pd = NULL)
  student_code <- state$get("student_code")
  if (fixed) {
    student_code <- clean_up(student_code)
    regex <- clean_up(regex)
  }
  num_hits <- get_num_hits(regex = regex, x = student_code, fixed = fixed)
  check_that(is_gte(num_hits, times), feedback = regex_state$details)
}
