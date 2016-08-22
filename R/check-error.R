#' Check whether the student's submission threw an error.
#'
#' With information gathered from the R Backend, detect
#' whether the student's submission generated an error.
#'
#' @param state State to start from (for \code{check_error})
#' @param ... S3 stuff
#' 
#' @examples
#' \dontrun{
#' # Example student code: x <- 4 + "a"
#' 
#' # SCT option 1
#' test_error()
#' 
#' # SCT option 2
#' ex() %>% check_error()
#' }
#'
#' @rdname test_error

#' @rdname test_error
#' @export
test_error <- function() {
  ex() %>% check_error()
}

#' @rdname test_error
#' @export
check_error.default <- function(state, ...) {
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
  error_state$add_details(type = "error", message = fb_msg, pd = pd)
  check_that(is_true(length(error_indices) == 0), feedback = error_state$details)
  return(error_state)
}
