#' Test whether a student correctly used an inline operator
#' 
#' @param name Name of the operator as a string, e.g. \code{"+"}
#' @param index integer that specifies which \code{name} operator that will be
#'   checked in the solution
#' @param eval whether or not to evaluate the expression that is formed by the
#'   operator
#' @param eq_condition  character vector indicating how to perform the
#'  comparison for each argument. See \code{\link{is_equal}}
#' @param not_called_msg custom feedback message in case the student did not
#'   call the operator often enough.
#' @param incorrect_msg custom feedback message in case the student's evaluation
#'   of the expression that is formed by the operator does not correspond with
#'   the corresponding call in the solution
#' @inheritParams test_function
#' 
#' 
#' @examples
#' \dontrun{
#' # Suppose the solution contains: 4 + 5
#' 
#' # To test this submission, provide the following in the sct
#' test_operator("+")
#' }
#' 
#' @export
test_operator <- function(name,
                          index = 1,
                          eval = TRUE,
                          eq_condition = "equivalent",
                          not_called_msg = NULL,
                          incorrect_msg = NULL) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  init_tags(fun = "test_operator")
  
  student_ops <- find_operator(student_pd, name)
  solution_ops <- find_operator(solution_pd, name)
  n_student_ops <- length(student_ops)
  n_solution_ops <- length(solution_ops)
  
  if  (index > length(solution_ops)) {
    stop(sprintf("There aren't %s %s operators available in the solution.", index, name))
  }
  solution_op <- solution_ops[[index]]
  
  if (eval) {
    sol_result <- try(base::eval(parse(text = solution_op$call), envir = solution_env), silent = TRUE)
    if (inherits(sol_result, "try-error")) {
      stop("Running ", solution_op$call, " generated an error: ", attr(sol_result, "cond")$message)
    }
  }
  
  if (is.null(not_called_msg)) {
    not_called_msg <- sprintf("The system wants to check the %s `%s` operator in your code, but hasn't found it.", get_num(index), name)
  }
  check_that(is_gte(n_student_ops, index), list(message = not_called_msg))
  
  if (eval) {
    
    seq <- get_seq(name, stud_indices = 1:n_student_ops, sol_index = index)
    passed <- FALSE
    feedback <- NULL
  
    for (i in seq) {
      student_op <- student_ops[[i]]
      
      stud_result <- try(base::eval(parse(text = student_op$call), envir = student_env), silent = TRUE)

      if (!inherits(stud_result, "try-error") && is_equal(stud_result, sol_result, eq_condition)) {
        set_used(name, stud_index = i, sol_index = index)
        passed <- TRUE
        break
      } else {
        if (is.null(feedback)) {
          if (is.null(incorrect_msg)) {
            incorrect_msg <- sprintf("Have you correctly used the `%s` operator? The result isn't what we expected.", name)
          }
          feedback <- list(message = incorrect_msg,
                           line_start = student_op$line1,
                           line_end = student_op$line2,
                           column_start = student_op$col1,
                           column_end = student_op$col2)
        }
      }
    }
    
    if (!passed) {
      check_that(failure(), feedback = feedback)
    }
  }
}

# Find all operators in the parse data
#' @importFrom utils getParseText
find_operator <- function(pd, name) {
  parent_ids <- pd$parent[pd$text == name]
  lapply(parent_ids, function(id) {
    call <- getParseText(pd, id)
    line_info <- as.list(pd[pd$id == id, c("line1", "col1", "line2", "col2")])
    c(call = call, line_info)
  })
}
