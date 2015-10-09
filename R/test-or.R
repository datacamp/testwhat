#' @export
test_or <- function(..., incorrect_msg = NULL, env = parent.frame()) {
  input <- substitute(alist(...))
  input[[1]] <- NULL
  len <- length(input) 
  
  passes <- logical(len)
  rep <- get_reporter()
  
  for (i in 1:len) {
    code <- input[[i]]
    rep$be_silent()
    eval(code, envir = env)
    rep$be_loud()
    passes[i] <- !rep$silent_fail
  }
  
  if (!any(passes)) {
    if (is.null(incorrect_msg)) {
      first <- input[[1]]
      eval(first, envir = env)
    } else {
      test_what(fail(), incorrect_msg)
    }
  }
}