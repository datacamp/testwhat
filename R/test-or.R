#' Test if one of the given sct parts are correct. 
#' 
#' Test if one of the given SCT code batteries are evaluated as being
#' correct. If not, the feedback message of the first fail is standardly
#' given. Can be used nested.
#' 
#' \itemize{
#'  \item{\code{...}: an arbritrary amount of code blocks containing SCT 
#'  code. \code{test_or} will check if one of the code blocks results in 
#'  a successful SCT evaluation.}
#' }

#' @param ... one of these code blocks with tests should succeed 
#' @param incorrect_msg msg displayed when none succeeds
#' @param choose_feedback choose feedback of test with this index
#' @param subs substitute content of ...
#' @param env environment in which to execute tests.
#' 
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
test_or <- function(..., incorrect_msg = NULL, choose_feedback = 1, 
                    subs = TRUE, env = parent.frame()) {
  if (subs) {
    input <- substitute(alist(...))
    input[[1]] <- NULL
  } else {
    input <- list(...)
  }
  len <- length(input) 
  
  passes <- logical(len)
  rep <- get_reporter()
  
  for (i in 1:len) {
    code <- input[[i]]
    rep$be_silent()
    run_until_fail(code, env)
    passes[i] <- !rep$get_silent_fail()
    rep$be_loud()
    if (passes[i]) break
  }
  
  if (!any(passes)) {
    if (is.null(incorrect_msg)) {
      first <- input[[choose_feedback]]
      run_until_fail(first, env)
    } else {
      test_what(fail(), incorrect_msg)
    }
  }
}