#' Test R object existence and value
#'
#' Test whether a student defined a certain object. If this is the case, and
#' if \code{eval} is \code{TRUE}, also check whether the value of the object
#' matches that of the solution.
#'
#' @param name name of the object to test.
#' @param eq_condition character string indicating how to compare. Possible values 
#' are \code{"equivalent"} (the default), \code{"equal"} and \code{"identical"}.
#' See \code{\link{expect_equivalent}}, \code{\link{expect_equal}}, 
#' and \code{\link{expect_identical}}, respectively.
#' @param eval Next to existence, check if the value of the object corresponds
#' between student en solution environment.
#' @param undefined_msg Optional feedback message in case the student did not define
#' the object. A meaningful message is automatically generated if not supplied.
#' @param incorrect_msg optional feedback message in case the student's object is not
#' the same as in the sample solution. Only used if \code{eval} is \code{TRUE}. 
#' A meaningful message is automatically generated if not supplied.
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
  
  stopifnot(exists(name, envir =  solution_env, inherits = FALSE))
  solution <- get(name, envir = solution_env, inherits = FALSE)
  
  if (is.null(undefined_msg)) {
    undefined_msg <-  build_object_undefined_msg(name)
  }

  line_info <- extract_object_assignment(student_pd, name)
  defined <- exists(name, envir = student_env, inherits = FALSE)
  test_what(expect_true(defined), c(list(message = undefined_msg), line_info))
  
  if (eval) {
    student <- get(name, envir = student_env, inherits = FALSE)
    
    eq_fun <- switch(eq_condition, equivalent = expect_equivalent,
                                   identical = expect_identical,
                                   equal = expect_equal,
                                   stop("invalid equality condition"))
    
    if (is.null(incorrect_msg)) {
      incorrect_msg <- build_object_incorrect_msg(name)
    }
    
    feedback <- c(list(message = incorrect_msg), line_info)
    rep <- get_reporter()
    rep$be_silent()
    ok <- run_until_fail(test_what(eq_fun(student, solution), feedback))
    rep$be_loud()
    if (!ok) {
      test_what(expect_true(any(class(student) %in% class(solution))), feedback)
      test_what(eq_fun(student, solution), feedback)
    }
  }
}


# alias <- function(name) UseMethod("alias")
# 
# alias.default <- function(name) {
#   sprintf("variable %s", name)
# }
# 
# alias.data.frame <- function(name) {
#   sprintf("data frame %s", name)
# }
# 
# alias.list <- function(name) {
#   sprintf("list %s", name)
# }
# 
# alias.character <- function(name) {
#   
# }
