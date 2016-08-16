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
  op <- ex() %>% test_op(name, index = index, not_called_msg = not_called_msg)
  if (eval) {
    op %>% test_equal(eq_condition = eq_condition, 
                      incorrect_msg = incorrect_msg)
  }
}

test_op <- function(state, name, index = 1, not_called_msg = NULL) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  
  op_state <- OperationState$new(state)
  op_state$add_details(type = "operator",
                        case = "called",
                        name = name,
                        index = index,
                        message = not_called_msg,
                        pd = NULL)
  
  student_ops <- find_operator(student_pd, name)
  solution_ops <- find_operator(solution_pd, name)
  
  check_sufficient(solution_ops, index, name)
  solution_op <- solution_ops[[index]]
  
  check_that(is_true(length(student_ops) >= index), feedback = op_state$details)
  
  # update the case for future tests
  op_state$set_details(case = "correct",
                       message = NULL)
  
  # manage blacklisting of operators
  state$update_blacklist()
  state$set(active_name = name)
  state$set(active_sol_index = index)
  options <- state$get_options(length(student_ops))
  
  student_ops[-options] <- NULL
  op_state$set(solution_op = solution_op)
  op_state$set(student_ops = student_ops)
  return(op_state)
}

#' @export
test_equal.OperationState <- function(state, eq_condition = "equivalent", incorrect_msg = NULL) {
  
  
  solution_op <- state$get("solution_op")
  student_ops <- state$get("student_ops")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  state$add_details(type = "operator",
                    case = "equal",
                    eval = eval,
                    eq_condition = eq_condition,
                    message = incorrect_msg)
  
  
  # Test if the specified arguments are correctly called
  sol_res <- tryCatch(base::eval(parse(text = solution_op$call), envir = solution_env), error = function(e) e)
  if (inherits(sol_res, 'error')) {
    stop(sprintf("Running %s gave an error: %s", solution_op$call, sol_res$message))
  }
  
  res <- numeric()
  details <- NULL
  for (i in seq_along(student_ops)) {
    student_op <- student_ops[[i]]
    if (is.null(student_op)) next
    
    stud_res <- tryCatch(base::eval(parse(text = student_op$call), envir = student_env), error = function(e) tryerrorstring)
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      state$set_details(student = stud_res,
                        solution = sol_res,
                        pd = student_op$pd)
      details <- state$details
    }
    
    # Check if the function arguments correspond
    if (is_equal(stud_res, sol_res, eq_condition)) {
      state$log(index = i, arg = 'none', success = TRUE)
      res <- c(res, i)
    } else {
      state$log(index = i, arg = 'none', success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- state$details
  }
  
  check_that(is_gte(length(res), 1), feedback = details)
  return(state)
}

# Find all operators in the parse data
#' @importFrom utils getParseText
find_operator <- function(pd, name) {
  parent_ids <- pd$parent[pd$text == name]
  lapply(parent_ids, function(id) {
    call <- getParseText(pd, id)
    pd <- pd[pd$id == id, ]
    list(call = call, pd = pd)
  })
}

