#' Test R Markdown file
#'
#' Test a single R Markdown file
#' 
#' This test is implemented using \code{\link{test_that}}.
#' This test should be called when there are multiple files in the submission.
#' 
#' @param code the SCT code for the file
#' @param student_file the name of the student file to be tested
#' @param solution_file the name of the solution file to be tested
#' @param student_code the entire code that has been submitted by the student. 
#' @param solution_code  the entire solution code.
#' @param env The environment in which the code should be tested.
#'
#' @export
test_rmd_file = function(code, student_file = NULL, solution_file = NULL, student_code = get_student_code(), solution_code = get_solution_code(), env = parent.frame()) {
  code <- substitute(code)
  if (is.character(code)) code <- parse(text = code)
  
  # get the entire student code and solution code and reset it on exit.
  # also remove the parse data that might have been saved to the sct env.
  on.exit({ 
    set_student_code(student_code)
    set_solution_code(solution_code)
  })
  
  if(is.null(student_file)) {
    student_file = names(student_code)[grepl(".rmd|.Rmd", names(student_code))]
    if(length(student_file) != 1) {
      stop("no or too many .Rmd files were found in the student code.")
    }
  }
  if(is.null(solution_file)) {
    solution_file = names(solution_code)[grepl(".rmd|.Rmd", names(solution_code))]
    if(length(solution_file) != 1) {
      stop("no or too many .Rmd files were found in the solution code.")
    }
  }
  
  if(!student_file %in% names(student_code)) {
    stop("student file name was not found in student code")
  } 
  if(!solution_file %in% names(solution_code)) {
    stop("solution file name was not found in solution code")
  }
  
  print("STUDENT CODE BEFORE = ")
  print(get_student_code())
  print("SOLUTION CODE BEFORE = ")
  print(get_solution_code())
  
  set_student_code(student_code[student_file])
  set_solution_code(solution_code[solution_file])
  
  print("STUDENT CODE AFTER = ")
  print(get_student_code())
  print("SOLUTION CODE AFTER = ")
  print(get_solution_code())
  
  eval(code, envir = env)
}