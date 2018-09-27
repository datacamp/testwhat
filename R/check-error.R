#' Explicitly check whether the student's submission threw an error.
#'
#' With information gathered from the R Backend, testwhat can detect whether the
#' student's submission generated an error.
#'
#' If all SCTs for an exercise pass, before marking the submission as correct
#' testwhat will automatically check whether the student submission generated an
#' error, unless the exercise explicitly allows for errors. This means it is not
#' needed to use \code{check_error} explicitly. However, in some cases,
#' using \code{check_error} explicitly somewhere
#' throughout your SCT execution can be helpful:
#'
#' \itemize{
#' \item{If you want to make sure people didn't write typos when
#' writing a long function name.}
#' \item{If you want to first verify whether a
#' function call actually runs,before checking whether the arguments were
#' specified correctly.}
#' \item{More generally, if, because of the content, it's
#' instrumental that the script runs without errors before doing any other
#' verifications.}
#' }
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
#' # SCT that explicitly checks for an error first
#' ex() %>% check_error()
#' ex() %>% check_object('x') %>% check_equal()
#' 
#' # SCT that does not have to check for an error
#' # testwhat will verify for an error implicitly
#' ex() %>% check_object('x') %>% check_equal()
#' }
#'
#' @name check_error

#' @rdname check_error
#' @export
check_error <- function(state, ...) {
  UseMethod("check_error", state)
}

#' @rdname check_error
#' @export
check_error.default <- function(state, incorrect_msg = NULL, ...) {
  assert_state(state)
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
  fail_if_v2_only()
  ex() %>% check_error(incorrect_msg = incorrect_msg)
}
