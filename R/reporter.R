#' DataCamp reporter: 'log' test results
#'
#' @importFrom R6 R6Class
DC_reporter <- R6::R6Class("DC_reporter",
  public = list(

    initialize = function() {},
  
    register_feedback = function(feedback) {
      if (private$silent == 0) {
        private$feedback <- feedback
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
    
    generate_feedback = function() {
      feedback <- private$feedback
      if (is.null(feedback)) {
        return(list(correct = TRUE,
                    message = to_html(private$success_msg)))
      } else {
        
        build_feedback_message(feedback)
        line_info <- get_line_info(feedback)
        
        if (is.null(line_info)) {
          return(list(correct = FALSE,
                      message = to_html(msg)))
        } else {
          return(c(list(correct = FALSE,
                        message = to_html(msg)),
                        line_info))
        }
      }
    }
  ),
  
  private = list(
    silent = 0,
    success_msg = sample(c("Good Job!", "Well done!", "Great work!"), 1),
    feedback = NULL
  )
)


get_line_info <- function(feedback) {
  
  # take 'highest pd' in list of feedback
  pd <- NULL
  for (i in length(feedback):1) {
    if (!is.null(feedback[["pd"]])) {
      pd <- feedback[["pd"]]
      break
    }
  }
  
  if (is.null(pd) || is.na(pd)) {
    return(NULL)
  }
  
  id <- pd$id[!(pd$parent %in% pd$id)]
  if (length(id) > 1) {
    return(list(line_start = min(pd$line1),
                column_start = min(pd$col1),
                line_end = max(pd$line2),
                column_end = max(pd$col2)))
  }
  x <- as.list(pd[pd$id == id, c("line1", "col1", "line2", "col2")])
  names(x) <- c("line_start", "column_start", "line_end", "column_end")
  x
}

#' @importFrom markdown markdownToHTML
to_html <- function(x) {
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  gsub("<p>(.*?)</p>", "\\1", html) #remove <p> tags, coded by front end.
}

select_info <- function(x, col) {
  sapply(x, `[[`, col)
}