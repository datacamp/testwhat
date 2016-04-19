#' DataCamp reporter: gather test results
#'
#' @export
#' @importFrom R6 R6Class
DataCampReporter <- R6::R6Class("DataCampReporter", inherit = testthat::Reporter,
public = list(
  
    add_result = function(context, test, result) {
      if (testthat:::expectation_broken(result) && private$silent == 0) {
        private$failures <- c(private$failures, list(test))
      }
    },
  
    be_silent = function() {
      private$silent <- private$silent + 1
    },

    be_loud = function() {
      private$silent <- max(0, private$silent - 1)
    },
    
    set_success_msg = function(msg) {
      private$success_msg <- msg
    },
    
    end_reporter = function() {
      failures <- private$failures
      if (length(failures) == 0) {
        return(list(correct = TRUE,
                    message = to_html(private$success_msg)))
      } else {
        failure <- failures[[1]]
        failure$message <- to_html(failure$message)
        return(c(list(correct = FALSE), failure))
      }
    }
  ),
  
  private = list(
    silent = 0,
    success_msg = sample(c("Good Job!", "Well done!", "Great work!"), 1),
    failures = list()
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