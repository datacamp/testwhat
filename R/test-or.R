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
#' 
#' @examples
#' \dontrun{
#'   # test if either the object a or the object b is correct
#'   test_or(test_object("a"), test_object("b"))
#' }
#'
#' @export
test_or <- function(..., incorrect_msg = NULL, choose_feedback = 1) {
  test_env <- tw$get("test_env")
  
  input <- substitute(alist(...))
  input[[1]] <- NULL
  
  len <- length(input) 
  
  passes <- logical(len)
  rep <- get_rep()
  
  for (i in 1:len) {
    code <- input[[i]]
    rep$be_silent()
    passes[i] <- run_until_fail(code)
    rep$be_loud()
  }
  
  if (!any(passes)) {
    if (is.null(incorrect_msg)) {
      failing_test <- input[[choose_feedback]]
      eval(failing_test, envir = test_env)
    } else {
      check_that(failure(), incorrect_msg)
    }
  }
}