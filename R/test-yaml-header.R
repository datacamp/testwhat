#' Test yaml header (Markdown)
#'
#' Test whether the student specified the correct options in the yaml header (for 
#' R Markdown exercises). This test should be called outside an test_rmd_group call.
#'
#' @param options  Set of options. Embedded options have to be specified using the dot notation.
#' @param check_equality whether or not to actually check the value assigned to the option (default TRUE)
#' @param allow_extra  whether or not the definition of additional options is accepted (default TRUE)
#' @param not_called_msg feedback message if option was not specified (optional but recommended)
#' @param incorrect_msg  feedback message if option was incorrectly set (optional but recommended)
#'
#' @import datacampAPI
#' @import testthat
#' @export
test_yaml_header <- function(options = NULL,
                               check_equality = TRUE,
                               allow_extra = TRUE,
                               not_called_msg = NULL,
                               incorrect_msg = NULL) {
  
  chunk_number <- tw$get("chunk_number")
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  init_tags(fun = "test_yaml_header")
  
  require(rmarkdown)
    
  yaml_solution <- try(unlist(rmarkdown:::parse_yaml_front_matter(strsplit(solution_code, split = "\n")[[1]])))
  if(inherits(yaml_solution, "try-error")) {
    stop("Something wrong with yaml header of solution code!")
  }
  
  yaml_student <- try(unlist(rmarkdown:::parse_yaml_front_matter(strsplit(student_code, split = "\n")[[1]])))
  test_what(expect_false(inherits(yaml_student, "try-error")), "Make sure the YAML header contains no errors. Beware of erroneous indentation.")
  
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
  test_what(expect_false(no_nas), not_called_msg)
  
  if(!no_nas && check_equality) {
    test_what(expect_equal(sol_options_select, stud_options_select), incorrect_msg)
  }

  if(!allow_extra) {
    test_what(expect_equal(length(stud_options_select), length(yaml_student)), incorrect_msg)
  }
}
