#' Combine tests
#'
#' \code{check_correct} checks whether a set of tests passes, and does
#' additional, more precise tests if these tests fail. In addition to the state,
#' it takes two code chunks; \itemize{\item{\code{check_code}: specifies the
#' code that checks on the (typically, final results of the) student's code.
#' These tests are executed silently, without the reporter generating
#' information for these.} \item{\code{diagnose_code}: Set of tests that gets
#' executed if the tests in \code{check_code} fail. These tests contain more
#' detailed tests, to pinpoint the problem.} }
#'
#' \code{check_correct} increases the flexibility for the student: if the tests
#' in \code{check_code} pass, the results of the tests in \code{diagnose_code}
#' are not considered. If you test for the end result in \code{check_code}, and
#' only do more rigorous testing in \code{diagnose_code}, you can allow for
#' multiple solutions to a challenge.
#'
#' Similarly, \code{check_or} checks whether one of many test sets pass. That
#' way, you can allow for multiple solutions.
#'
#' Both \code{check_or} and \code{check_correct} makes the state you feed it to
#' its subtests available as \code{.} (the dot), similar to how magrittr does
#' it.
#'
#' @param state The state. Typically \code{\link{ex}} but can also be a
#'   lower-level state if you're using nested \code{check_or}s or
#'   \code{check_correct}s
#' @param ... sets of tests. In the case of \code{check_correct}, the first set
#'   is the \code{check_code}, the second set is the \code{diagnose_code}. For
#'   \code{check_or}, an unrestricted number of sets of tests: only one of these
#'   tests has to pass for the \code{check_or} to pass.
#'
#' @examples
#' \dontrun{
#' # Example 1 solution code
#' x <- mean(1:3)
#'
#' # Example SCT
#' ex() %>% check_correct(
#'   check_object(., "x") %>% check_equal(),
#'   check_fun(., "mean") %>% check_arg("x") %>% check_equal()
#' )
#'
#' # Following submissions will all be accepted:
#' x <- mean(1:3)
#' x <- 2
#' x <- mean(-1:5)
#'
#' # Example 2 solution code
#' # a <- 3; b <- 4
#'
#' # Example SCT
#' ex() %>% check_or(
#'   check_object(., 'a') %>% check_equal(),
#'   check_object(., 'b') %>% checK-equal()
#' )
#'
#' # Following submissions will all be accepted:
#' a <- 3; b <- 4
#' a <- 3
#' b <- 4
#' }
#'
#' @rdname check_logic


#' @name check_logic
#' @export
check_correct <- function(state, ...) {
  if (nargs() == 3) {
    # If three inputs, the first one must be a state
    set_dot(state)
    test_correct(..., v2_check = FALSE)
  } else {
    fail_if_v2_only(errmsg = 'check_correct() can only be used with a state as the first argument, e.g. ex() %>% check_corrrect(...).')
    # Else, fall back on old behavior
    input <- as.list(substitute(list(...)))
    test_correct(substitute(state), input[[2]], sub = FALSE)
  }
}

#' @name check_logic
#' @export
check_or <- function(state, ...) {
  if (class(substitute(state)) == "name") {
    # If something was piped in, it will be a . (class name, done by magrittr)
    set_dot(state)
    test_or(..., v2_check = FALSE)
  } else {
    fail_if_v2_only(errmsg = 'check_or() can only be used with a state as the first argument, e.g. ex() %>% check_or(...).')
    # Else, fall back on previous behavior
    tests <- as.list(substitute(list(...)))
    tests[[1]] <- substitute(state)
    do.call(test_or, c(tests))
  }
}

set_dot <- function(x) {
  assign(".", x, envir = tw$get("state")$get("test_env"))
}

test_correct <- function(check_code, diagnose_code, sub = TRUE, v2_check = TRUE) {
  if(v2_check) {
    fail_if_v2_only(errmsg = 'test_correct() can no longer be used in SCTs. Use ex() %>% check_correct() instead.')
  }
  if(sub) {
    check_code <- substitute(check_code)
    diagnose_code <- substitute(diagnose_code)
  }
  check_res <- run_until_fail(check_code)
  diagnose_res <- run_until_fail(diagnose_code)
  if (check_res$correct) {
    # all good
  } else {
    if (diagnose_res$correct) {
      check_that(failure(), feedback = check_res$feedback)
    } else {
      check_that(failure(), feedback = diagnose_res$feedback)
    }
  }
  return(invisible(NULL))
}

test_or <- function(..., incorrect_msg = NULL, choose_feedback = 1, v2_check = TRUE) {
  if(v2_check) {
    fail_if_v2_only(errmsg = 'test_or() can no longer be used in SCTs. Use ex() %>% check_or() instead.')
  }
  input <- as.list(substitute(list(...)))
  input[[1]] <- NULL
  
  passes <- logical(length(input) )
  feedback <- list()
  
  for (i in seq_along(input)) {
    code <- input[[i]]
    res <- run_until_fail(code)
    passes[i] <- res$correct
    feedback[[i]] <- res$feedback
  }
  
  if (!any(passes)) {
    if (is.null(incorrect_msg)) {
      check_that(failure(), feedback = feedback[[choose_feedback]])
    } else {
      check_that(failure(), feedback = incorrect_msg)
    }
  }
  return(invisible(NULL))
}