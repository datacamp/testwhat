#' DataCamp reporter: gather test results along with elapsed time and
#' feedback messages.
#'
#' This reporter gathers all results, adding additional information such as
#' test elapsed time and feedback messages.
#'
#' @export
#' @importFrom R6 R6Class
DataCampReporter <- R6::R6Class("DataCampReporter", inherit = testthat::Reporter,
  public = list(
    failures = list(),
    silent = 0,
    feedback = list(),
    success_msg = sample(c("Good Job!", "Well done!", "Great work!"), 1),

    set_success_msg = function(msg = "") {
      self$success_msg <- msg
    },
    
    add_result = function(context, test, result) {
      if (testthat:::expectation_broken(result)) {
        self$failures <- c(self$failures, list(test))
      }
    },
    
    end_test = function(context, test) {
      failures <- self$failures
      if (length(failures) == 0) {
        return()
      } else {
        stop(sct_failed_msg)
      }
    },

    ### new methods
    be_silent = function() {
      self$silent <- self$silent + 1
    },

    be_loud = function() {
      self$silent <- max(0, self$silent - 1)
    },
    
    get_outcome = function() {
      failures <- self$failures
      if (length(failures) == 0) {
        return(list(correct = TRUE,
                    message = to_html(self$success_msg)))
      } else {
        failure <- failures[[1]]
        failure$message <- to_html(failure$message)
        return(c(list(correct = FALSE), failure))
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