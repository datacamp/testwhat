#' Test R Markdown file
#'
#' Test a single R Markdown file
#' 
#' This test should be called when there are multiple files in the submission.
#' 
#' @param code the SCT code for the file
#' @param student_file the name of the student file to be tested
#' @param solution_file the name of the solution file to be tested
#' @inheritParams test_function
#' 
#' @rdname rstudio
#' @keywords internal
test_rmd_file <- function(code, 
                          student_file = NULL, 
                          solution_file = NULL) {
  old_state <- ex()
  on.exit(tw$set(state = old_state))
  test_env <- old_state$get("test_env")
  
  tw$set(state = get_rmd_file(old_state, student_file, solution_file))
  eval(substitute(code), envir = test_env)
}

get_rmd_file <- function(state, student_file, solution_file) {
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
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
  
  file_state <- SubState$new(state)
  file_state$set(student_code = student_code[student_file],
                 solution_code = solution_code[solution_file])
  return(file_state)
}