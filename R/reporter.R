#' @importFrom R6 R6Class
DC_reporter <- R6::R6Class("DC_reporter",
  public = list(

    initialize = function() {},
  
    register_feedback = function(feedback) {
      private$feedback <- feedback
      stop(sct_failed_msg)
    },
    
    set_success_msg = function(msg) {
      private$success_msg <- msg
    },
    
    get_feedback = function() {
      feedback <- private$feedback %||% private$success_msg
      private$feedback <- NULL
      return(feedback)
    }
  ),
  
  private = list(
    success_msg = sample(c("Good Job!", "Well done!", "Great work!"), 1),
    feedback = NULL
  )
)

generate_payload <- function(feedback, correct, ex_type) {
  msg <- to_html(build_feedback_message(feedback))
  payload <- list(correct = correct,
                  message = msg)
  
  if (!correct && ex_type != "MarkdownExercise") {
    line_info <- get_line_info(feedback)  
    if (!is.null(line_info)) {
      payload <- c(payload, line_info)
    }
  }
  
  return(payload)
}

get_line_info <- function(feedback) {
  
  # take 'highest pd' in list of feedback
  pd <- NULL
  for (i in length(feedback):1) {
    if (!is.null(feedback[[i]][["pd"]])) {
      pd <- feedback[[i]][["pd"]]
      break
    }
  }
  
  if (!isTRUE(try(is.data.frame(pd), silent = TRUE))) {
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
