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

  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  student_pd <- tw$get("student_pd")
  init_tags(fun = "test_object")
  
  check_defined(name, solution_env)
  solution <- get(name, envir = solution_env, inherits = FALSE)
  
  if (is.null(undefined_msg)) {
    undefined_msg <-  build_object_undefined_msg(name)
  }

  line_info <- extract_object_assignment(student_pd, name)
  defined <- exists(name, envir = student_env, inherits = FALSE)
  check_that(is_true(defined), c(list(message = undefined_msg), line_info))
  
  if (eval) {
    student <- get(name, envir = student_env, inherits = FALSE)
    
    if (is.null(incorrect_msg)) {
      incorrect_msg <- paste0(build_object_incorrect_msg(name),
                              build_diff(sol = solution, stud = student,
                                         eq_condition = eq_condition,
                                         id = sprintf("`%s`", name)))
    }
    
    feedback <- c(list(message = incorrect_msg), line_info)
    rep <- get_rep()
    rep$be_silent()
    ok <- run_until_fail(check_that(is_equal(student, solution, eq_condition), feedback))
    rep$be_loud()
    if (!ok) {
      check_that(is_true(any(class(student) %in% class(solution))), feedback)
      check_that(is_true(is_equal(student, solution, eq_condition)), feedback)
    }
  }
}
