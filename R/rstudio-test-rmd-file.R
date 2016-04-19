#' Test R Markdown file
#'
#' Test a single R Markdown file
#' 
#' This test should be called when there are multiple files in the submission.
#' 
#' @param code the SCT code for the file
#' @param student_file the name of the student file to be tested
#' @param solution_file the name of the solution file to be tested
#' @param env The environment in which the code should be tested.
#' @inheritParams test_function
#' 
#' @export
test_rmd_file <- function(code, 
                          student_file = NULL, 
                          solution_file = NULL, 
                          env = parent.frame()) {
  
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  init_tags(fun = "test_rmd_file")
  
  code <- substitute(code)
  if (is.character(code)) code <- parse(text = code)
  
  # get the entire student code and solution code and reset it on exit.
  on.exit({ 
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
    tw$set(student_ds = NULL)
    tw$set(solution_ds = NULL)
  })
  
  if (is.null(student_file)) {
    student_file <- names(student_code)[grepl(".rmd|.Rmd", names(student_code))]
    if (length(student_file) != 1) {
      stop("no or too many .Rmd files were found in the student code.")
    }
  }
  if (is.null(solution_file)) {
    solution_file <- names(solution_code)[grepl(".rmd|.Rmd", names(solution_code))]
    if (length(solution_file) != 1) {
      stop("no or too many .Rmd files were found in the solution code.")
    }
  }

  if (!student_file %in% names(student_code)) {
    stop("student file name was not found in student code")
  } 
  if (!solution_file %in% names(solution_code)) {
    stop("solution file name was not found in solution code")
  }
  
  tw$set(student_code = student_code[student_file])
  tw$set(solution_code = solution_code[solution_file])
  
  eval(code, envir = env)
}