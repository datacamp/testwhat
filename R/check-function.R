#' Check whether a student correctly called a function/operator
#'
#' Check whether a student called a function correctly. Note:
#' \code{test_function} and \code{test_function_v2} are now identical and either
#' can be used.
#'
#' @param state state to start from
#' @param name  name of the function/operator as a string, e.g. \code{"mean"} or
#'   \code{"+"}
#' @param index  integer that specifies which call of \code{name} in the
#'   solution code will be checked.
#' @param eval  logical vector indicating whether and how to compare arguments.
#'   If \code{eval} is \code{NA}, student and solution argument are not
#'   compared. If \code{eval} is \code{FALSE}, the string versions of the
#'   arguments are compared. If \code{eval} is \code{TRUE}, the argument in the
#'   student code is evaluated in the student environment and the argument in
#'   the solution code is evaluated in the solution environment, and their
#'   results are compared. Setting this to \code{FALSE} can be useful, e.g., to
#'   check whether the student supplied a large predefined object, or when
#'   you're in a sub-SCT where the environments are not unambiguously available.
#' @param eq_condition  character vector indicating how to perform the
#'   comparison for each argument. See \code{\link{is_equal}}
#' @param eq_fun optional argument to specify a custom equality function. The
#'   function should take two arguments and always return a single boolean
#'   value: \code{TRUE} or \code{FALSE}.
#' @param not_called_msg custom feedback message in case the student did not
#'   call the function often enough.
#' @param incorrect_msg custom feedback message in case the student did not call
#'   the function with the same argument values as in the sample solution. You
#'   can specify a vector of arguments with the same length as \code{args}, to
#'   have argument-specific custom feedback.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states
#' @param ... S3 stuff
#' @param arg_not_specified_msg custom message in case argument was not
#'   specified (for \code{check_arg})
#' @param arg name of argument to specify (for \code{check_arg})
#'
#' @examples
#' \dontrun{
#' # Example 1
#' mean(1:3)
#'
#' # SCT
#' ex() %>% check_function("mean") %>% check_arg("x") %>% check_equal()
#'
#' # Example 2
#' mean(c(NA, 1, 2), na.rm = TRUE)
#'
#' # SCT
#' ex() %>% check_function("mean") %>% {
#'   check_arg(., "x") %>% check_equal()
#'   check_arg(., "na.rm") %>% check_equal()
#' }
#'
#' # Example 3
#' 5 + 4
#'
#' # SCT
#' ex() %>% check_operator("+") %>% check_result() %>% check_equal()
#' }
#'
#' @name check_function

#' @rdname check_function
#' @export
check_function <- function(state, name, index = 1, not_called_msg = NULL, append = TRUE) {
  assert_state(state)
  state$assert_is_not("ObjectState", "check_object")
  check_fun_op_helper(state, name = name, index = index, not_called_msg = not_called_msg, append = append, type = "function")
}

#' @rdname check_function
#' @export
check_operator <- function(state, name, index = 1, append = TRUE, not_called_msg = NULL) {
  assert_state(state)
  state$assert_is_not("ObjectState", "check_object")
  check_fun_op_helper(state, name = name, index = index, not_called_msg = not_called_msg, append = append, type = "operator")
}

check_fun_op_helper <- function(state, name, index, not_called_msg, append, type = c("function", "operator")) {

  type <- match.arg(type)
  finder <- switch(type, `function` = find_function_calls, operator = find_operators)
  CallState <- switch(type, `function` = FunctionState, operator = OperationState)

  call_state <- CallState$new(state)
  call_state$add_details(type = type,
                         case = "called",
                         name = name,
                         index = index,
                         message = not_called_msg,
                         append = append,
                         pd = NULL)

  student_calls <- finder(pd = state$get("student_pd"),
                          name = name,
                          env = state$get("student_env"))
  solution_calls <- finder(pd = state$get("solution_pd"),
                           name = name,
                           env = state$get("solution_env"))

  check_sufficient(solution_calls, index, name)
  solution_call <- solution_calls[[index]]

  check_that(is_true(length(student_calls) >= index), feedback = call_state$details)
  student_calls <- student_calls[index]

  # update the case for future tests
  call_state$set_details(case = "correct",
                         message = NULL)

  call_state$set(solution_call = solution_call)
  call_state$set(student_calls = student_calls)
  
  return(call_state)
}

#' @rdname check_function
#' @export
check_arg <- function(state, arg, arg_not_specified_msg = NULL, append = TRUE) {
  assert_state(state)
  state$assert_is("CallState", "check_function")

  solution_call <- state$get("solution_call")
  student_call <- state$get("student_calls")[[1]]

  arg_state <- ArgumentState$new(state)
  arg_state$add_details(type = "argument",
                        case = "specified",
                        name = arg,
                        message = arg_not_specified_msg,
                        pd = student_call$pd,
                        append = append)

  if (! arg %in% names(solution_call$args)) {
    stop(" Make sure that the arguments you specify are actually specified",
         " by the corresponding function call in the solution code.")
  }

  # Check if the function is called with the right arg
  check_that(is_true(arg %in% names(student_call$args)), feedback = arg_state$details)
  
  sol_arg = solution_call$args[[arg]]
  stu_arg = student_call$args[[arg]]
  arg_state$set(solution_arg = sol_arg)
  arg_state$set(solution_pd = sol_arg$pd)
  arg_state$set(solution_code = deparse(sol_arg$expr))
  arg_state$set(student_arg = stu_arg)
  arg_state$set(student_pd = stu_arg$pd)
  arg_state$set(student_code = deparse(stu_arg$expr))

  arg_state$set_details(case = "correct",
                        message = NULL)

  return(arg_state)
}


#' @rdname check_function
#' @export
check_equal.ArgumentState <- function(state, incorrect_msg = NULL, eval = TRUE, eq_condition = "equivalent", eq_fun = NULL, append = TRUE, ...) {
  assert_state(state)
  solution_arg <- state$get("solution_arg")
  student_arg <- state$get("student_arg")

  state$add_details(type = "argument",
                    case = "equal",
                    eval = eval,
                    eq_condition = eq_condition,
                    message = incorrect_msg,
                    append = append)

  # Evaluate the solution argument and see if no error
  solution_obj <- eval_argument(solution_arg,
                                eval = eval,
                                env = state$get("solution_env"))

  if (isTRUE(try(all.equal(solution_obj, tryerrorstring), silent = TRUE))) {
    stop("check_equal() found an argument that causes an error when evaluated.")
  }

  if (is.null(eq_fun)) {
    eq_fun <- function(x, y) is_equal(x, y, eq_condition)
  }

  # Evaluate the student argument
  student_obj <- eval_argument(student_arg,
                               eval = eval,
                               env = state$get("student_env"))
  
  state$set_details(student = student_obj,
                    solution = solution_obj,
                    is_dots = is_dots(student_arg))
  
  check_that(is_true(eq_fun(student_obj, solution_obj)), feedback = state$details)
  
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

## Deprecated functions

test_function <- function(name,
                          args = NULL,
                          index = 1,
                          eval = TRUE,
                          eq_condition = "equivalent",
                          not_called_msg = NULL,
                          args_not_specified_msg = NULL,
                          incorrect_msg = NULL) {
  fail_if_v2_only()
  n_args <- length(args)
  if (!is.null(args_not_specified_msg) && length(args_not_specified_msg) < n_args) {
    args_not_specified_msg <- rep(args_not_specified_msg[1], n_args)
  }
  if (!is.null(incorrect_msg) && length(incorrect_msg) < n_args) {
    incorrect_msg <- rep(incorrect_msg[1], n_args)
  }
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  
  fun_state <- check_function(ex(), name, index = index,
                              not_called_msg = not_called_msg,
                              append = is.null(not_called_msg))
  for (i in seq_along(args)) {
    arg_state <- check_arg(fun_state,
                           arg = args[i],
                           arg_not_specified_msg = args_not_specified_msg[i],
                           append = is.null(args_not_specified_msg))
    if (!is.na(eval[i])) {
      check_equal(arg_state,
                  incorrect_msg = incorrect_msg[i],
                  eval = eval[i],
                  eq_condition = eq_condition[i],
                  append = is.null(incorrect_msg[i]))
    }
  }
}

test_function_v2 <- test_function