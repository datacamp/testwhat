#' Test equality (general)
#' 
#' 
#' @param state state to start from
#' @param ... arguments passed to S3 implementations
#' @param eq_condition how to compare (\code{"equivalent"}, \code{"equal"} or \code{"identical"})
#' @param incorrect_msg custom message in case the objects to do not correspond
#' 
#' @export
test_equal <- function(state, ...) {
  UseMethod("test_equal", state)
}

#' @rdname test_equal
#' @export
test_equal.default <- function(state, ...) {
  stop("Can't run test_equal() with a ", class(state)[1], " as input state.", call. = FALSE)  
}

#' @rdname test_equal
#' @export
test_equal.ObjectState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  test_equal_helper(state, incorrect_msg = incorrect_msg, eq_condition = eq_condition, type = "object")
}

#' @rdname test_equal
#' @export
test_equal.ObjectColumnState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  test_equal_helper(state, incorrect_msg = incorrect_msg, eq_condition = eq_condition, type = "column")
}

#' @rdname test_equal
#' @export
test_equal.ObjectElementState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  test_equal_helper(state, incorrect_msg = incorrect_msg, eq_condition = eq_condition, type = "element")
}

#' @rdname test_equal
#' @export
test_equal.FunctionResultState <- function(state, eq_condition = "equivalent", incorrect_msg = NULL) {
  test_call_result_equal(state, eq_condition = eq_condition, incorrect_msg = incorrect_msg, type = "function")
}

#' @rdname test_equal
#' @export
test_equal.OperationResultState <- function(state, eq_condition = "equivalent", incorrect_msg = NULL) {
  test_call_result_equal(state, eq_condition = eq_condition, incorrect_msg = incorrect_msg, type = "operator")
}

#' @rdname test_equal
#' @export
test_equal.ExprResultState <- function(state, incorrect_msg = NULL, eq_condition = "equivalent") {
  fundef_test_equal_helper(state, incorrect_msg, eq_condition, type = "result")
}

#' @rdname test_equal
#' @export
test_equal.ExprOutputState <- function(state, incorrect_msg = NULL) {
  return(fundef_test_equal_helper(state, incorrect_msg, type = "output"))
}

#' @rdname test_equal
#' @export
test_equal.ExprErrorState <- function(state, incorrect_msg = NULL) {
  return(fundef_test_equal_helper(state, incorrect_msg, type = "error"))
}


#' @param eval how to evaluate the arguments
#' @rdname test_equal
#' @export
test_equal.ArgumentState <- function(state, incorrect_msg = NULL, eval = TRUE, eq_condition = "equivalent") {
  
  solution_arg <- state$get("solution_arg")
  student_args <- state$get("student_args")
  
  state$add_details(type = "argument",
                    case = "equal",
                    eval = eval,
                    eq_condition = eq_condition,
                    message = incorrect_msg)
  
  # Test if the specified arguments are correctly called
  solution_obj <- eval_argument(solution_arg,
                                eval = eval, 
                                env = state$get("solution_env"))
  
  if (isTRUE(try(all.equal(solution_obj, tryerrorstring), silent = TRUE))) {
    stop("test_equal() found an argument that causes an error when evaluated.")
  }
  
  res <- logical(length(student_args))
  details <- NULL
  for (i in seq_along(student_args)) {
    student_arg <- student_args[[i]]
    if (isTRUE(is.na(student_arg))) next
    student_obj <- eval_argument(student_arg,
                                 eval = eval,
                                 env = state$get("student_env"))
    
    # If no hits, use details of the first try
    if (is.null(details)) {
      if (is_dots(student_arg)) {
        pd <- state$student_calls[[i]]$pd
        is_dots <- TRUE
      } else {
        pd <- student_arg$pd
        is_dots <- FALSE
      }
      state$set_details(student = student_obj,
                        solution = solution_obj,
                        pd = pd,
                        is_dots = is_dots)
      details <- state$details
    }
    
    # Check if the function arguments correspond
    if (is_equal(student_obj, solution_obj, eq_condition)) {
      state$log(index = i, success = TRUE)
      res[i] <- TRUE
    } else {
      state$log(index = i, success = FALSE)
    }
  }
  
  if (is.null(details)) {
    details <- state$details
  }
  
  check_that(is_gte(sum(res), 1), feedback = details)
  
  return(state)
}

tryerrorstring <- "try-error-in-evaluation"

eval_argument <- function(arg, eval, env) {
  if (is_dots(arg)) {
    return(lapply(arg, eval_argument, eval, env))
  }
  if (eval) {
    obj <- try(eval(arg$expr, envir = env), silent = TRUE)
    if (inherits(obj, "try-error")) {
      obj <- tryerrorstring
    }
  } else {
    obj <- stringify(arg$expr)
  }
  return(obj)
}

is_dots <- function(arg) {
  is.list(arg) && ! "expr" %in% names(arg)
}

stringify <- function(arg) {
  x <- deparse(arg)
  x <- gsub("\\s|;", "", x)
  x <- gsub("'", "\"", x)
  x <- gsub("FALSE", "F", x)
  x <- gsub("TRUE", "T", x)
  return(x)
}




