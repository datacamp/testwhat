#' Check whether the student's submission threw an error.
#'
#' With information gathered from the R Backend, \code{test_error} detects
#' whether the student's submission generated an error. Automatically, a feedback is generated,
#' which can be appended with an additional incorrect_msg.
#'
#' @param state state to start from (can be missing)
#' @param ... arguments that are used to build a function definition call
#' @param no_error_msg custom message in case the expression or function call didn't generate an error
#' 
#' @examples
#' \dontrun{
#' # Example student code: x <- 4 + "a"
#' 
#' # R error message as feedback:
#' test_error()
#' }
#'
#' @export
test_error <- function(state, ...) {
  if (missing(state)) {
    test_error(ex())
  } else {
    UseMethod("test_error", state)  
  }
}

#' @rdname test_error
#' @export
test_error.default <- function(state) {
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


#' @rdname test_error
#' @export
test_error.ExprState <- function(state, no_error_msg = NULL) {
  expr <- state$get("expr")
  run_expr_error_helper(state, 
                        expr = expr,
                        expr_str = as.character(expr),
                        no_error_msg = no_error_msg)
}

#' @rdname test_error
#' @export
test_error.FunDefState <- function(state, ..., no_error_msg = NULL) {
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  run_expr_error_helper(state, 
                        expr = parse(text = expr_str),
                        expr_str = expr_str,
                        no_error_msg = no_error_msg)
}