#' Test whether the student specified the correct options in the yaml header (R Markdown)
#'
#' Test whether the student specified the correct options in the yaml header (R Markdown)
#'
#' This test is implemented using \code{\link{test_that}}.
#' This test should be called outside an test_rmd_group call.
#'
#' @param options  Set of options. Embedded options have to be specified using the dot notation.
#' @param check_equality whether or not to actually check the value assigned to the option (default TRUE)
#' @param allow_extra  whether or not the definition of additional options is accepted (default TRUE)
#' @param student_code character string containing the entire student code.
#' @param solution_code  character string containing the entire solution code.
#' @param not_called_msg feedback message if option was not specified.
#' @param incorrect_msg  feedback message if option was incorrectly set.
#'
#' @import datacampAPI
#' @export
test_yaml_header <- function(options = NULL,
                               check_equality = TRUE,
                               allow_extra = TRUE,
                               chunk_number = get_chunk_number(),
                               student_code = get_student_code(),
                               solution_code = get_solution_code(),
                               not_called_msg = NULL,
                               incorrect_msg = NULL) {
  require(rmarkdown)
  yaml_student = unlist(rmarkdown:::parse_yaml_front_matter(strsplit(student_code, split = "\n")[[1]]))
  yaml_solution = unlist(rmarkdown:::parse_yaml_front_matter(strsplit(solution_code, split = "\n")[[1]]))
    
  test_that("In the yaml header, the options are set correctly", {
    if(is.null(options)) {
      options <- names(yaml_solution)
      if(length(options) == 0) {
        return(TRUE)
      }
    } 
    
    # Set up default messages
    # message if specified function was not called
    if(is.null(not_called_msg)) {
      not_called_msg <- sprintf("The YAML header of your submission should contain the option%s %s.", 
                               if(length(options) == 1) "" else "s", collapse_props(options))
    }
     
    # message if the properties are not found or set incorrectly
    if(is.null(incorrect_msg)) {
      incorrect_msg = sprintf("In your YAML header, correctly define the option%s %s.",
                              if(length(options) == 1) "" else "s", collapse_props(options))
      if(!allow_extra)
        incorrect_msg = paste(incorrect_msg, "Do not define any other options!")
    }
    
    # select from sol_options and stud_props the ones to check on
    sol_options_select = yaml_solution[options]
    stud_options_select = yaml_student[options]
    if(any(is.na(names(sol_options_select)))) {
      stop(sprintf("You want to test on yaml options that are not in the solution's yaml header", chunk_number))
    }
    
    # check if all options available
    expect_that(any(is.na(names(stud_options_select))), is_false(), failure_msg = not_called_msg)
    
    if(check_equality) {
      # check the equality of stud and solution options.
      n_common = length(intersect(stud_options_select, sol_options_select))
      expect_that(length(sol_options_select), equals(n_common), failure_msg = incorrect_msg)
    }

    if(!allow_extra) {
      expect_that(length(stud_options_select), equals(length(stud_options)), failure_msg = incorrect_msg)
    }
  })
}
