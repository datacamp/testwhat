#' Try to get code from files submitted by student.
#' 
#' @param state a
#' @param name full file name (e.g. script.R)
#' @param missing_msg feedback to give if file is missing
#' 
#' @export
check_file.FileSysState <- function(state, name, missing_msg = NULL, parse = TRUE) {
  stu_code <- state$get("student_code")
  sol_code <- state$get("solution_code")

  if (is.null(sol_file <- sol_code[[name]])) stop(
    sprintf("file name %s not in %s", name, paste(names(sol_code), collapse = ", "))
    )

  c_state <- ChildState$new(state)
  c_state$add_details(type = "file",
                      case = "available",
                      folder = "."           # TODO: get actual folder?
                      )
  
  # TODO remove custom feedback and see if messages.R comes through!
  check_that(is_true(!is.null(stu_file <- stu_code[[name]])),
             feedback = sprintf("file name %s not in %s", name, paste(names(stu_code)))
             )
  
  c_state$set(student_code = stu_file, solution_code = sol_file)
  
  if (parse) c_state$set(student_pd = build_pd(stu_file),
                         solution_pd = build_pd(sol_file))

  c_state
}