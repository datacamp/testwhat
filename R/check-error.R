#' Check whether the student's submission threw an error.
#' 
#' With information gathered from the R Backend, detect whether the student's
#' submission generated an error. If this is the case a feedback message will be
#' generated that contains the R error in a formatted style. You can use
#' \code{incorrect_msg} to add a custom message to this error.
#' 
#' @param state State to start from (for \code{check_error})
#' @param incorrect_msg additional message that is appended to the automatically
#'   generated feedback message.
#' @param ... S3 stuff
#'   
#' @examples
#' \dontrun{
#' # Example student code: x <- 4 + "a"
#' 
#' # SCT
#' ex() %>% check_error()
#' }
#' 
#' @export
check_error.default <- function(state, incorrect_msg = NULL, ...) {
  output_list <- state$get("output_list")
  student_pd <- state$get("student_pd")
  
  error_state <- ErrorState$new(state)

  error_indices <- which(sapply(output_list, `[[`, "type") == "r-error")
  if(length(error_indices) == 0) {
    fb_msg <- "ok"
    pd <- NULL
  } else {
    error_index <- error_indices[1]
    error <- output_list[[error_index]]$payload
    fb_msg <- paste("Your code contains an error that you should fix:",
                    "```",
                    error,
                    "```",
                    ifelse(is.null(incorrect_msg), "", incorrect_msg),
                    sep = "\n")
    
    line_info <- NULL
    call_index <- error_indices[1] - 1
    if(call_index > 0) {
      call <- output_list[[call_index]]$payload
      hits <- student_pd$text == call
      if(!any(hits)) {
        pd <- NULL
      } else if (sum(hits) == 1) {
        pd <- student_pd[hits, ]
      } else {
        # more than 1 hit - select the code that actually generated the error
        hit_indices <- which(sapply(output_list, function(x) {
          x$type == "code" && x$payload == call
        }))
        pd <- student_pd[which(hits)[call_index == hit_indices], ]
      }
    } else {
      pd <- NULL
    }
  }
  error_state$add_details(type = "error", message = fb_msg, pd = pd, append = TRUE)
  check_that(is_true(length(error_indices) == 0), feedback = error_state$details)
  return(error_state)
}

# Deprecated
test_error <- function(incorrect_msg = NULL) {
  ex() %>% check_error(incorrect_msg = incorrect_msg)
}
