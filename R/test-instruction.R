#' Test a single instruction of the challenges interface
#' 
#' See vignette for more information.
#' 
#' @param index the instruction index
#' @param code the test code for that instruction
#' @param env environment in which to execute tests.
#'
#' @export
test_instruction = function(index, code, env = parent.frame()) {
  code <- substitute(code)
  if (is.character(code)) code <- parse(text = code)
  
  get_reporter()$set_instruction_index(as.integer(index))
  eval_fail <- try(eval(code, envir = env), silent = TRUE)
  if (inherits(eval_fail, "try-error")) {
    cond <- attr(eval_fail, "condition")$message
    if (!identical(cond, sct_failed_msg)) {
      stop(cond)
    }
  }
}