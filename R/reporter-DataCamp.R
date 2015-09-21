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
    continue = "logical",
    start_test_time = "proc_time",
    report = "character",
    results = "list",
    current_test_results = "list",
    silent = "logical",
    silent_fail = "logical",
    instruction_index = "numeric",
    inh_failure = "logical",
    inh_failure_msg = "character"),

  methods = list(
    ### overriden methods from Reporter
    initialize = function(...) {
      report <<- "first"
      callSuper(...)
    },
    start_reporter = function(...) {
      callSuper(...)
      results <<- list()
      silent <<- FALSE
      instruction_index <<- 0
      inh_failure <<- FALSE
      inh_failure_msg <<- ""
      current_test_results <<- list()
    },

    toggle_inherit_failure = function(toggle_to = FALSE, toggle_msg = NULL) {
      inh_failure <<- toggle_to
      if (!is.null(toggle_msg) && is.character(toggle_msg)) {
        inh_failure_msg <<- toggle_msg
      } else {
        inh_failure_msg <<- ""
      }
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
                          instruction_index = instruction_index,
                          results = current_test_results)
        results <<- c(results, list(test_info))
      }
      current_test_results <<- list()

      callSuper() # at the end because it resets the test name
    },

    add_result = function(result) {
      if(inh_failure) {
        result$failure_msg <- inh_failure_msg
      }
      
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
          continue <<- (report == "all" || report == "challenge")
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
    
    ## challenge methods
    set_instruction_index = function(index) {
      instruction_index <<- index
    },
    
    ## summary and feedback messages

    get_summary = function() {
      # Summarize the individuals test results into a data frame
      rows <- lapply(results, summarize_test_results)
      summary <- do.call(rbind, rows)
      # Ensure the order of the tests with factor levels
      summary
    },

    get_feedback = function(failure_msg = NULL, success_msg = NULL) {
      summary <- get_summary()

      passed <- !failed  # TRUE if there are no tests
      if (report == "first") {
        if (passed) {
          if (is.null(success_msg)) success_msg <- sample(.praise, 1)
          feedback <- success_msg
        } else {
          first <- match(FALSE, summary$passed)
          feedback <- summary$feedback[first]  
        }
        return(list(passed = passed, feedback = feedback))
      } else if (report == "all") {
        if(passed) {
          if (is.null(success_msg)) success_msg <- sample(.praise, 1)
          feedback <- success_msg
        } else {
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
        }
        return(list(passed = passed, feedback = feedback))
      } else if (report == "challenge") {
        n_inst <- max(summary$instruction_index)
        if(passed) {
          if (is.null(success_msg)) success_msg <- sample(.praise, 1)
          feedback <- success_msg
          passed_steps <- rep(TRUE, n_inst)
        } else {
          passed_steps <- sapply(1:n_inst, function(x) all(summary[summary$instruction_index == x, "passed"]))
          max_passed = suppressWarnings(max(which(passed_steps)))
          if(max_passed == length(passed_steps)) {
            passed <- TRUE
            if (is.null(success_msg)) success_msg <- sample(.praise, 1)
            feedback <- success_msg
            passed_steps <- rep(TRUE, n_inst)
          } else if(max_passed == -Inf) {
            # nothing correct
            feedback <- "Keep trying, you'll get there!"
            passed_steps <- rep(FALSE, n_inst)
          } else {
            # some things correct
            # passed_steps <- passed_steps | c(rep(TRUE, max_passed - 1), rep(FALSE, n_inst - max_passed + 1))
            feedback <- "Keep trying, you'll get there!"
          }
        }
        return(list(passed = passed, feedback = feedback, passed_steps = head(passed_steps, n_inst - 1)))
      } else {
        stop("type of reporting not implemented")
      }
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

  data.frame(context = context, passed = passed,
             error = error, feedback = feedback, user = test$user,
             system = test$system, real = test$real, 
             instruction_index = test$instruction_index,
             stringsAsFactors = FALSE)
}

# Error message when a test is stopped after the first failure
get_stop_msg <- function() "**test stopped because of failure**"


.praise <- c(
  "You rock!",
  "You are a coding rockstar!",
  "Keep up the good work.",
  ":)",
  "Woot!",
  "Way to go!",
  "Nice code."
)