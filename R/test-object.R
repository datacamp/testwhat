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
  
  if (is.null(name)) {
    stop("argument \"name\" is missing, with no default")
  }
  
  stopifnot(exists(name, envir =  solution_env, inherits = FALSE))
  solution <- get(name, envir = solution_env, inherits = FALSE)
  
  if (is.null(undefined_msg)) {
    undefined_msg <- build_undefined_object_msg(name)
  }

  line_info <- get_assignment(name, student_pd)
  defined <- exists(name, envir = student_env, inherits = FALSE)
  test_what(expect_true(defined), c(list(message = undefined_msg), line_info))
  
  if (defined && eval) {
    student <- get(name, envir = student_env, inherits = FALSE)
    eq_fun <- switch(eq_condition, equivalent = expect_equivalent,
                                   identical = expect_identical,
                                   equal = expect_equal,
                                   like = expect_like,
                                   stop("invalid equality condition"))
    
    # set_tags(auto_feedback = is.null(incorrect_msg))
    if (is.null(incorrect_msg)) {
      incorrect_msg <- build_incorrect_object_msg(name)
    }
    
    # set_tags(test = "correct")
    test_what(eq_fun(student, solution), c(list(message = incorrect_msg), line_info))
  }
}

get_assignment <- function(name, pd) {
  symbols <- pd[pd$token == "SYMBOL" & pd$text == name, ]
  assigns <- pd[pd$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN"), ]
  
  assign_calls <- list()
  for(i in 1:nrow(assigns)) {
    assign <- assigns[i, ]
    children <- get_children(pd, assign$parent)
    hit <- intersect(children, symbols$id)
    if(length(hit) != 1) {
      next
    } else {
      comp <- switch(assign$token, LEFT_ASSIGN = `<`, RIGHT_ASSIGN = `>`)
      if(comp(hit, assign$id)) {
        parent <- pd[pd$id == assign$parent, ]
        line_info <- as.list(parent[c("line1", "col1", "line2", "col2")])
        names(line_info) <- c("line_start", "column_start", "line_end", "column_end")
        assign_calls <- c(assign_calls, list(line_info))
      } else {
        next
      }
    }
  }
  
  # for now, only pass line info if there's only assignment
  if(length(assign_calls) == 1) {
    return(assign_calls[[1]])
  } else {
    return(NULL)
  }
  return(assign_calls)
}
