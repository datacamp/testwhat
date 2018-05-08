#' Test a single R Markdown file group (R Markdown exercises)
#'
#' Test a single R Markdown file group (R Markdown exercises) with arbitrary testwhat functions.
#'
#' @param group_number  Number of the group.
#' @param code  SCT code to test the group (in curly braces)
#' @keywords internal
test_rmd_group <- function(group_number, code) {
  old_state <- ex()
  on.exit(tw$set(state = old_state))
  test_env <- old_state$get("test_env")
  tw$set(state = get_rmd_group(old_state, group_number))
  eval(substitute(code), envir = test_env)
}

get_rmd_group <- function(state, group_number) {
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")

  group_state <- MarkdownState$new(state)
  group_state <- parse_docs(group_state)
  
  solution_ds <- group_state$get("solution_ds")
  student_ds <- group_state$get("student_ds")
  
  if (group_number > length(solution_ds)) {
    stop(sprintf("Invalid group_number (%s), while solution contains only %s parts",
                 group_number,length(solution_ds)))
  }
  
  student_ds_part <- student_ds[[group_number]]
  solution_ds_part <- solution_ds[[group_number]]
  group_state$set(student_ds_part = student_ds_part,
                  solution_ds_part = solution_ds_part,
                  student_code = student_ds_part$input,
                  solution_code = solution_ds_part$input)
  
  # set numbers, to be used in default messages of tests
  if (class(student_ds_part) == "block") {
    group_state$set(chunk_number = group_number - sum(sapply(student_ds[1:group_number],class) == "inline"),
                    student_pd = build_pd(student_ds_part$input),
                    solution_pd = build_pd(solution_ds_part$input))
  } else if (class(student_ds_part) == "inline") {
    group_state$set(inline_number = group_number - sum(sapply(student_ds[1:group_number],class) == "block"))
  }
  return(group_state)
}

