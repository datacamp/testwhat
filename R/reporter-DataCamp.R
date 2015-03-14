#' @include reporter.r
NULL


setOldClass('proc_time')

#' DataCamp reporter: gather test results along with elapsed time and
#' feedback messages.
#'
#' This reporter gathers all results, adding additional information such as
#' test elapsed time and feedback messages.
#'
#' @export
#' @export DataCampReporter
#' @aliases DataCampReporter
#' @keywords debugging
DataCampReporter <- setRefClass(
  "DataCampReporter", contains = "Reporter",
  fields = list(
    start_test_time = "proc_time",
    report = "character",
    results = "list",
    current_test_results = "list",
    silent = "logical",
    silent_fail = "logical"),

  methods = list(
    ### overriden methods from Reporter
    initialize = function(...) {
      report <<- "first"
      silent <<- FALSE
      callSuper(...)
    },
    start_reporter = function(...) {
      callSuper(...)
      results <<- list()
      current_test_results <<- list()
    },

    start_test = function(desc) {
      callSuper(desc)
      current_test_results <<- list()
      start_test_time <<- proc.time()
    },

    end_test = function() {
      if(length(current_test_results) == 0) {
        results <<- list()
      } else {
        el <- as.double(proc.time() - start_test_time)
        test_info <- list(context = context, test = test,
                          user = el[1], system = el[2], real = el[3],
                          results = current_test_results)
        results <<- c(results, list(test_info))
      }
      current_test_results <<- list()

      callSuper() # at the end because it resets the test name
    },

    add_result = function(result) {
      if(silent) {
        if(!result$passed) {
          silent_fail <<- TRUE
          if (!result$error) stop(get_stop_msg())
        }
      } else {
        callSuper(result)
        current_test_results <<- c(current_test_results, list(result))

        # Stop the test after the first failure by throwing an error.
        # Of course the error shouldn't be thrown if the result was an
        # error in the first place.
        if (!result$passed) {
          continue <<- report == "all"
          failed <<- TRUE
          if (!result$error) stop(get_stop_msg())
        }
      }
    },

    ### new methods
    be_silent = function() {
      silent <<- TRUE
      silent_fail <<- FALSE
    },

    be_loud = function() {
      silent <<- FALSE
    },

    get_summary = function() {
      # Summarize the individuals test results into a data frame
      rows <- lapply(results, summarize_test_results)
      summary <- do.call(rbind, rows)
      # Ensure the order of the tests with factor levels
      summary$test <- factor(summary$test, levels = unique(summary$test))
      summary
    },

    get_feedback = function(failure_msg = NULL, success_msg = NULL) {
      summary <- get_summary()

      passed <- !failed  # TRUE if there are no tests
      if (passed) {
        if (is.null(success_msg)) success_msg <- sample(.praise, 1)
        feedback <- success_msg
      } else if (report == "first") {
        first <- match(FALSE, summary$passed)
        feedback <- summary$feedback[first]
      } else if (report == "all") {
        summary_by_test <- split(summary, summary[, "test", drop=FALSE])
        n_tests <- length(summary_by_test)
        feedback <- lapply(summary_by_test, function(x) x$feedback[!x$passed])
        which_failed <- which(vapply(feedback, length, numeric(1)) > 0)
        n_failed <- length(which_failed)
        if (is.null(failure_msg)) {
          if (n_tests == 1) failure_msg <- "Your code failed our test."
          else {
            failure_msg <- sprintf("Your code failed %d out of %d tests.",
                                      n_failed, n_tests)
          }
          mistake_text <- if (n_failed == 1) "mistake" else "mistakes"
          mistake_prefix <- sprintf("We found the following %s:", mistake_text)
          failure_msg <- paste(failure_msg, mistake_prefix)
        }
        feedback <- paste(seq_len(n_failed), feedback[which_failed], sep = ": ")
        feedback <- paste(feedback, collapse = "\n")
        feedback <- paste(failure_msg, feedback, sep = "\n")
      } else stop("type of reporting not implemented")

      list(passed = passed, feedback = feedback)
    }

  )
)


# Summarize the results from one test
summarize_test_results <- function(test) {
  test_results <- test$results
  n_subtests <- length(test_results)

  passed <- vapply(test_results, "[[", logical(1), "passed")
  error <- vapply(test_results, "[[", logical(1), "error")

  if (n_subtests == 0) {
    feedback <- character()
    context <- character()
  } else {
    which_component <- ifelse(passed, "success_msg", "failure_msg")
    feedback <- mapply("[[", test_results, which_component)
    context <- if (length(test$context) == 0) "" else test$context
  }

  data.frame(context = context, test = test$test, passed = passed,
             error = error, feedback = feedback, user = test$user,
             system = test$system, real = test$real,
             stringsAsFactors = FALSE)
}


# Error message when a test is stopped after the first failure
get_stop_msg <- function() "**test stopped because of failure**"
