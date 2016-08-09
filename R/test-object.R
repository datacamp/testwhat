#' Test R object existence and value
#' 
#' Test whether a student defined a certain object. If this is the case, and if 
#' \code{eval} is \code{TRUE}, also check whether the value of the object 
#' matches that of the solution.
#' 
#' @param name name of the object to test.
#' @param eq_condition character string indicating how to compare. See
#'   \code{\link{is_equal}}.
#' @param eval Next to existence, check if the value of the object corresponds 
#'   between student en solution environment.
#' @param undefined_msg Optional feedback message in case the student did not 
#'   define the object. A meaningful message is automatically generated if not 
#'   supplied.
#' @param incorrect_msg optional feedback message in case the student's object 
#'   is not the same as in the sample solution. Only used if \code{eval} is 
#'   \code{TRUE}. A meaningful message is automatically generated if not 
#'   supplied.
#'   
#' @examples
#' \dontrun{
#' # Example 1 solution code:
#' # x <- mean(1:3, na.rm = TRUE)
#' 
#' # sct command to test existence and value of x:
#' test_object("x")
#' 
#' # sct command to test only existence of x:
#' test_object("x", eval = FALSE)
#' 
#' # Example 2 solution code:
#' # y <- list(a = 2, b = 3, c = 4)
#' 
#' # Small numerical difference allowed + no check on attributes
#' test_object(y)
#' 
#' # Small numerical difference allowed + check attributes
#' test_object(y, eq_condition = "equals")
#' 
#' # No numerical difference allowed + check attributes
#' test_object(y, eq_condtion = "identical")
#' }
#' 
#' @export
test_object <- function(name, eq_condition = "equivalent",
                        eval = TRUE,
                        undefined_msg = NULL, incorrect_msg = NULL) {

  obj_state <- ex() %>% test_obj(name, undefined_msg = undefined_msg)
  if (eval) {
    obj_state %>% test_equal(incorrect_msg = incorrect_msg, eq_condition = eq_condition)  
  }
}

#' Test R object existence
#'
#' @export
test_obj <- function(state, name, undefined_msg = NULL) {

  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")

  obj_state <- ObjectState$new(state)
  obj_state$add_details(type = "object",
                        case = "defined",
                        name = name,
                        message = undefined_msg,
                        pd = NULL)

  check_defined(name, solution_env)
  solution_object <- get(name, envir = solution_env, inherits = FALSE)

  check_that(is_true(exists(name, envir = student_env, inherits = FALSE)),
             feedback = obj_state$details)

  student_object <- get(name, envir = student_env, inherits = FALSE)

  obj_state$set(student_object = student_object,
                solution_object = solution_object,
                student_pd = extract_object_assignment(student_pd, name),
                solution_pd = extract_object_assignment(solution_pd, name))
  return(obj_state)
}

test_equal.ObjectState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  student_obj <- state$get("student_object")
  solution_obj <- state$get("solution_object")
  state$set_details(case = "equal",
                    student = student_obj,
                    solution = solution_obj,
                    eq_condition = eq_condition,
                    message = incorrect_msg,
                    pd = state$get("student_pd"))

  check_that(is_equal(student_obj, solution_obj, eq_condition),
             feedback = state$details)
  return(state)
}
