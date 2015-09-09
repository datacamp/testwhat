#' @include reporter.r
NULL

library(crayon)

#' Test reporter: stop on error.
#'
#' The default reporter, executed when \code{expect_that} is run
#' interactively, or when the test files are executed by R CMD check. It
#' responds by \link{stop}()ing on failures and doing nothing otherwise. This
#' will ensure that a failing test will raise an error.
#'
#' This should be used when doing a quick and dirty test, or during the final
#' automated testing of R CMD check.  Otherwise, use a reporter that runs all
#' tests and gives you more context about the problem.
#'
#' @export
#' @export FullTestReporter
#' @aliases FullTestReporter
#' @keywords debugging
#' @param ... Arguments used to initialise class
FullTestReporter <- setRefClass("FullTestReporter", contains = "Reporter",
                            fields = c("failures",
                                       "n_fails",
                                       "n_pass",
                                       "fail",
                                       "expected_fail_passed",
                                       "expected_fail_feedback",
                                       "expected_fail_msg"),
                            methods = list(
                              initialize = function(...) {
                                failures <<- list()
                                n_fails <<- 0
                                n_pass <<- 0
                                fail <<- FALSE
                                expected_fail_passed <<- FALSE
                                expected_fail_msg <<- NULL
                                expected_fail_feedback <<- list()
                                callSuper(...)
                              },
                              
                              start_high_level_test = function(desc) {
                                test <<- desc
                              },
                              
                              end_high_level_test = function() {
                                cur_test <- test
                                test <<- NULL
                                if (length(failures) == 0) {
                                  n_pass <<- n_pass + 1
                                  cat(green(paste0("\t✔\tPASSED: ", cur_test, "\n")))
                                  return()
                                } else {
                                  n_fails <<- n_fails + 1
                                  cat(red(paste0("\t✘\tFAILED: ", cur_test, "\n")))
                                  invisible(sapply(failures, function(failure) {
                                    cat(paste0("\t\t\t", red(bold(as.character(failure))), "\n"))
                                  }))
                                  failures <<- list()
                                }
                              },
                              
                              toggle_fail = function(new_fail, msg = NULL) {
                                fail <<- new_fail
                                if(isTRUE(fail)) {
                                  expected_fail_passed <<- FALSE
                                  expected_fail_feedback <<- list()
                                  expected_fail_msg <<- NULL
                                  if (!is.null(msg)) {
                                    expected_fail_msg <<- msg
                                  }
                                } else {
                                  if (!expected_fail_passed) {
                                    failures <<- c(failures, paste("Expected fail:",expected_fail_feedback))
                                  }
                                }
                              },
                              
                              add_result = function(result) {
                                if (isTRUE(fail)) {
                                  if (result$passed) {
                                    expected_fail_feedback <<- c(expected_fail_feedback, paste("Passed:",result$failure_msg))
                                  } else {
                                    if (is.null(expected_fail_msg) || length(grep(expected_fail_msg, result$failure_msg)) != 0) {
                                      expected_fail_passed <<- TRUE
                                    } else {
                                      expected_fail_feedback <<- c(expected_fail_feedback, paste(result$failure_msg, " - need fail msg:", expected_fail_msg))
                                    }
                                  }
                                } else {
                                  if (!result$passed) {
                                    failures <<- c(failures, result$failure_msg)
                                  }
                                }
                              },
                              
                              # Ignore this, these are for deeper tests. We don't need this output. It's confusing.
                              start_test = function(desc) {
                              },
                              
                              end_test = function() {
                              }
                            )
)
