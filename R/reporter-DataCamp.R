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
#' @importFrom methods setRefClass
#' @keywords debugging
DataCampReporter <- R6::R6Class("DataCampReporter", inherit = testthat::Reporter,
  public = list(
    failures = list(),
    silent = 0,
    feedback = list(),
    success_msg = sample(c("Good Job!", "Well done!", "Great work!"), 1),

    set_success_msg = function(msg = "") {
      self$success_msg <- msg
    },
    
    add_result = function(contest, test, result) {
      self$cat_tight("ADD RESULT!")
      if (testthat:::expectation_failur(result)) {
        self$failures <- c(self$failures, list(result))
        cat("Shit went wrong")
      } else if (testthat:::expectation_error(result)) {
        self$failures <- c(self$failures, list(result))
        cat("Shit threw an error")
      } else {
        self$successes <- c(self$successes, list(result))
        cat("All good")
      }
    },
#     add_result = function(context, test, result) {
#       str(result)
#       
#       if (silent == 0) {
#         self$results <- c(self$results, list(list(passed = result$passed,
#                                                   feedback = self$feedback)))
#       }
#       
#       if (!result$passed) {
#         stop(sct_failed_msg)
#       }
#     },

    ### new methods
    be_silent = function() {
      self$silent <- self$silent + 1
    },

    be_loud = function() {
      self$silent <- max(0, self$silent - 1)
    },
    
    
    get_outcome = function() {
      test_results <- select_info(self$results, "passed")
      if (!all(test_results)) {
        selector <- which(!test_results)[1]
        fb <- self$results[[selector]]$feedback
        fb$message <- to_html(fb$message)
        return(c(list(correct = FALSE), fb))
      } else {
        return(list(correct = TRUE, 
                    message = to_html(self$success_msg)))
      }
    }
  )
)


#' @importFrom markdown markdownToHTML
to_html <- function(x) {
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  gsub("<p>(.*?)</p>", "\\1", html) #remove <p> tags, coded by front end.
}

select_info <- function(x, col) {
  sapply(x, `[[`, col)
}