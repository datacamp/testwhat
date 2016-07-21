#' DataCamp reporter: gather test results
#'
#' @importFrom R6 R6Class
DC_reporter <- R6::R6Class("DC_reporter",
  public = list(

    initialize = function() {},
  
    set_feedback = function(msg) {
      if (private$silent == 0) {
        private$feedback <- msg
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
    
    get_feedback = function() {
      feedback <- private$feedback
      if (is.null(feedback)) {
        return(list(correct = TRUE,
                    message = to_html(private$success_msg)))
      } else {
        feedback$message <- to_html(feedback$message)
        return(c(list(correct = FALSE), feedback))
      }
    }
  ),
  
  private = list(
    silent = 0,
    success_msg = sample(c("Good Job!", "Well done!", "Great work!"), 1),
    feedback = NULL
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