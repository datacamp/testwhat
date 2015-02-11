#' Test a single R Markdown file group (R Markdown exercises)
#'
#' Test a single R Markdown file group (R Markdown exercises) with arbitrary testwhat functions.
#' This test is implemented using \code{\link{test_that}}. 
#'
#' @param group_number  Number of the group.
#' @param code  SCT code to test the group (in curly braces)
#' @param student_code the entire code that has been submitted by the student. 
#' @param solution_code  the entire solution code.
#' @param env The environment in which the code should be tested.
#'
#' @export
test_rmd_group = function(group_number, code, student_code = get_student_code(), solution_code = get_solution_code(), env = parent.frame()) {
  code <- substitute(code)
  if (is.character(code)) code <- parse(text = code)

  
  # get the entire student code and solution code and reset it on exit.
  # also remove the parse data that might have been saved to the sct env.
  on.exit({ 
    set_student_code(student_code)
    set_solution_code(solution_code)
    remove_student_pd()
    remove_solution_pd()
  })
  
  passed <- parse_docs()
  if(!isTRUE(passed)) {
    return(FALSE)
  }

  if(group_number > length(get_solution_ds())) {
    stop(sprintf("Invalid group_number (%s), while solution contains only %s parts",
                 group_number,length(get_solution_ds())))
  }
  
  set_student_ds_part(get_student_ds()[[group_number]])
  set_solution_ds_part(get_solution_ds()[[group_number]])
  set_student_code(get_student_ds_part()$input)  
  set_solution_code(get_solution_ds_part()$input)
  
  # set numbers, to be used in default messages of tests
  if(class(get_student_ds_part()) == "block") {
    set_chunk_number(group_number - sum(sapply(get_student_ds()[1:group_number],class) == "inline"))
  } else if(class(get_student_ds_part()) == "inline") {
    set_inline_number(group_number - sum(sapply(get_student_ds()[1:group_number],class) == "block"))
  }
  
  eval(code, envir = env)
}