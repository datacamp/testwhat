#' Test a single R Markdown file group (R Markdown exercises)
#'
#' Test a single R Markdown file group (R Markdown exercises) with arbitrary testwhat functions.
#' This test is implemented using \code{\link{test_what}}. 
#'
#' @param group_number  Number of the group.
#' @param code  SCT code to test the group (in curly braces)
#' @param env The environment in which the code should be tested.
#'
#' @export
test_rmd_group <- function(group_number, code, env = parent.frame()) {
  
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  solution_ds <- tw$get("solution_ds")
  student_ds <- tw$get("student_ds")
  init_tags(fun = "test_rmd_group")
  
  code <- substitute(code)
  if (is.character(code)) code <- parse(text = code)

  # get the entire student code and solution code and reset it on exit.
  on.exit({ 
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
    tw$set(student_pd = NULL)
    tw$set(solution_pd = NULL)
  })
  
  passed <- parse_docs()
  if (!isTRUE(passed)) {
    return(FALSE)
  }

  solution_ds <- tw$get("solution_ds")
  student_ds <- tw$get("student_ds")
  
  if (group_number > length(solution_ds)) {
    stop(sprintf("Invalid group_number (%s), while solution contains only %s parts",
                 group_number,length(solution_ds)))
  }
  
  student_ds_part <- student_ds[[group_number]]
  solution_ds_part <- solution_ds[[group_number]]
  tw$set(student_ds_part = student_ds_part)
  tw$set(solution_ds_part = solution_ds_part)
  tw$set(student_code = student_ds_part$input)
  tw$set(solution_code = solution_ds_part$input)
  
  # set numbers, to be used in default messages of tests
  if (class(student_ds_part) == "block") {
    tw$set(chunk_number = group_number - sum(sapply(student_ds[1:group_number],class) == "inline"))
    tw$set(student_pd = build_pd(student_ds_part$input))
    tw$set(solution_pd = build_pd(solution_ds_part$input))
  } else if (class(student_ds_part) == "inline") {
    tw$set(inline_number = group_number - sum(sapply(student_ds[1:group_number],class) == "block"))
  }
  
  eval(code, envir = env)
}