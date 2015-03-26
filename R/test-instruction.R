#' Test a single instruction of the challenges interface
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
  eval(code, envir = env)
}