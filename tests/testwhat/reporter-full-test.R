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
                                       "failed"),
                            methods = list(
                              initialize = function(...) {
                                failures <<- list()
                                n_fails <<- 0
                                n_pass <<- 0
                                failed <<- FALSE
                                callSuper(...)
                              },
                              
                              start_high_level_test = function(desc) {
                                failed <<- FALSE
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
                                    cat(paste0("\t\t\t", red(bold(as.character(failure$failure_msg))), "\n"))
                                  }))
                                  failures <<- list()
                                }
                                failed <<- FALSE
                              },
                              
                              add_result = function(result) {
                                if (result$passed) {
                                  failed <<- FALSE
                                } else {
                                  failed <<- TRUE
                                  failures <<- c(failures, list(result))
                                }
                              },
                              
                              # Ignore this, these are for deeper tests. We don't need this output. It's confusing.
                              start_test = function(desc) {
                              },
                              
                              end_test = function() {
                              }
                            )
)
