#' Test whether a student correctly used an inline operator
#' 
#' @examples
#' \dontrun{
#' # Todo add examples
#' }
#'
#' @export
test_operator <- function(name,
                          index = 1,
                          eval = TRUE,
                          eq_condition = "equivalent",
                          not_called_msg = NULL, 
                          error_msg = NULL,
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
  test_what(expect_true(n_student_ops >= index), list(message = not_called_msg))
  
  if (eval) {
    
    seq <- get_seq(name, stud_indices = 1:n_student_ops, sol_index = index)
    passed <- FALSE
    feedback <- NULL
  
    for (i in seq) {
      student_op <- student_ops[[i]]
      
      stud_result <- try(base::eval(parse(text = student_op$call), envir = student_env), silent = TRUE)

      if (!inherits(stud_result, "try-error") && is_equal(stud_result, sol_result, condition = eq_condition)) {
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
      test_what(fail(), feedback = feedback)
    }
  }
}

# Find all operators in the parse data
find_operator <- function(pd, name) {
  parent_ids <- pd$parent[pd$text == name]
  lapply(parent_ids, function(id) {
    call <- getParseText(pd, id)
    line_info <- as.list(pd[pd$id == id, c("line1", "col1", "line2", "col2")])
    c(call = call, line_info)
  })
}
