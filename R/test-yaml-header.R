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
#' @param chunk_number The chunk number being treated (set automatically)
#' @param student_code character string containing the entire student code (set automatically)
#' @param solution_code  character string containing the entire solution code (set automatically)
#' @param not_called_msg feedback message if option was not specified (optional but recommended)
#' @param incorrect_msg  feedback message if option was incorrectly set (optional but recommended)
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
    
  test_that("In the yaml header, the options are set correctly", {
    yaml_student <- try(unlist(rmarkdown:::parse_yaml_front_matter(strsplit(student_code, split = "\n")[[1]])))
    expect_that(inherits(yaml_student, "try-error"), is_false(), failure_msg = "Make sure the YAML header contains no errors. Beware of erroneous indentation.")
    
    yaml_solution <- try(unlist(rmarkdown:::parse_yaml_front_matter(strsplit(solution_code, split = "\n")[[1]])))
    if(inherits(yaml_solution, "try-error")) {
      stop("Something wrong with yaml header of solution code!")
    }
    
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
    
    no_nas = any(is.na(names(stud_options_select)))
    # check if all options available
    expect_that(no_nas, is_false(), failure_msg = not_called_msg)
    
    if(!no_nas && check_equality) {
      expect_equal(sol_options_select, stud_options_select, failure_msg = incorrect_msg)
    }

    if(!allow_extra) {
      expect_that(length(stud_options_select), equals(length(stud_options)), failure_msg = incorrect_msg)
    }
  })
}
