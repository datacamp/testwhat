#' Check whether the student defined the correct chunk options (R Markdown
#' exercises)
#' 
#' Check whether the student defined the correct chunk options in an R Markdown
#' exercise
#' 
#' This test can only be called inside a test_rmd_group() call!
#' 
#' @param options  Set of options
#' @param not_called_msg feedback message if option was not specified
#' @param incorrect_msg  feedback message if option was incorrectly set
#' @keywords internal
test_chunk_options <- function(options = NULL,
                               not_called_msg = NULL,
                               incorrect_msg = NULL) {
  state <- ex()
  chunk_number <- state$get("chunk_number")
  student_chunk <- state$get("student_ds_part")
  solution_chunk <- state$get("solution_ds_part")
  
  # First, check if both student and solution chunk are 'block' class (i.e. code chunks)
  if (class(solution_chunk) != "block") {
    stop("The specified rmd group is not of 'block' class.")
  }
  
  check_that(is_equal(class(student_chunk), "block"), "Wrong class student chunk")
  
  sol_options <- solution_chunk$params
  stud_options <- student_chunk$params
  
  if (is.null(options)) {
    options <- names(sol_options)
    if (length(options) == 0) {
      return(TRUE)
    }
  } 
  
  # Set up default messages
  # message if specified function was not called
  if (is.null(not_called_msg)) {
    not_called_msg = sprintf("Code chunk %i of your submission should contain the option%s %s.", 
                             chunk_number, if (length(options) == 1) "" else "s", collapse_props(options))
  }
  
  # message if the properties or not found or set incorrectly
  if (is.null(incorrect_msg)) {
    incorrect_msg = sprintf("In code chunk %i of your submission, make sure to correctly define the option%s %s.",
                            chunk_number, if (length(options) == 1) "" else "s", collapse_props(options))
  }
      
  # select from sol_options and stud_props the ones to check on
  #
  # reverse the list, because in case options are defined multiple times,
  # the last options are the ones that are seen as valid by RMarkdown
  sol_options_select = rev(sol_options)[options]
  stud_options_select = rev(stud_options)[options]
  if (any(is.na(names(sol_options_select)))) {
      stop(sprintf("You defined options that are not in code chunk %i of the solution", chunk_number))
  }
  
  no_nas <- any(is.na(names(stud_options_select)))
  # check if all options available
  check_that(is_false(no_nas), not_called_msg)
  
  
  # check the equality of stud and solution options.
  if (!no_nas) {
    check_that(is_equal(sol_options_select, stud_options_select), incorrect_msg)
  }
}

