#' Test whether a student correctly called a function 
#'
#' Test whether a student called a function, possibly with certain arguments, 
#' correctly. Note that \code{test_function} and \code{test_function_v2} are now
#' identical and either can be used, although the latter is likely to be phased
#' out in the future.
#' 
#' @param name  name of the function to test.
#' @param args  character vector of argument names that the student should have
#' supplied in the function calls.
#' @param index  integer that specifies which call of \code{name} in the solution 
#' code will be checked.
#' @param eval  logical vector indicating whether the corresponding argument
#' should be evaluated before testing. Setting this to \code{FALSE} can be
#' useful, e.g., to test whether the student supplied a large predefined
#' object, as only the corresponding \code{\link{name}} is compared in this
#' case (use with care!).
#' @param eq_condition  character vector indicating how to perform the
#' comparison for each argument. See \code{\link{is_equal}}
#' @param not_called_msg custom feedback message in case the student did not call the
#' function often enough.
#' @param args_not_specified_msg custom feedback message in case the student did call the function
#' with the arguments listed in \code{args}
#' @param incorrect_msg custom feedback message in case the student did not call the
#' function with the same argument values as in the sample solution. 
#' You can specify a vector of arguments with the same length as \code{args}, to have
#' argument-specific custom feedback.
#' 
#' @examples
#' \dontrun{
#' # Suppose the solution contains: mean(1:3, na.rm = TRUE)
#' # To test this submission, provide the following in the sct
#' test_function("mean", c("x", "na.rm"))
#' }
#'
#' @export
test_function <- function(name, 
                          args = NULL, 
                          index = 1,
                          eval = TRUE,
                          eq_condition = "equivalent",
                          not_called_msg = NULL, 
                          args_not_specified_msg = NULL,
                          incorrect_msg = NULL) {
  
  n_args <- length(args)
  if (!is.null(incorrect_msg) && length(incorrect_msg) < n_args) {
    incorrect_msg <- rep(incorrect_msg[1], n_args)
  }
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  
  fun_state <- ex() %>% test_fun(name, index = index, not_called_msg = not_called_msg)
  for (i in seq_along(args)) {
    fun_state %>% 
      test_arg(args[i], arg_not_specified_msg = args_not_specified_msg) %>% 
      test_equal(incorrect_msg = incorrect_msg[i], eval = eval[i], eq_condition = eq_condition[i])
  }
}

#' @rdname test_function
#' @export
test_function_v2 <- test_function

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
#' @param error_msg custom feedback message in case the operation by the student caused an error.
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
                          error_msg = NULL,
                          incorrect_msg = NULL) {
  op <- ex() %>% test_op(name, index = index, not_called_msg = not_called_msg)
  if (eval) {
    op %>%
      test_result(error_msg = error_msg) %>%
      test_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg)
  }
}

#' Check the result of a function call
#' 
#' \code{test_function_result} finds a function specified by \code{name} in both the
#' student and solution code, and then compares the result of calling these functions.
#' Compared to \code{\link{test_function}}, the arguments of the function call are not checked,
#' only the result.
#' 
#' @param name name of the function whose output you would like to check.
#' @param index Ordinal number of the call you want to check (both student and solution!).
#' @param not_called_msg feedback message in case the function is not retrieved.
#' @param eval_error_msg feedback message in case the student function call at the mentioned index generated an error.
#' @param incorrect_msg  feedback message in case the evaluation was not the same as in the solution.
#' @inheritParams test_function
#'
#' @export
test_function_result <- function(name = NULL,
                                 index = 1,
                                 eq_condition = "equivalent",
                                 not_called_msg = NULL,
                                 eval_error_msg = NULL,
                                 ordered = TRUE,
                                 incorrect_msg = NULL) {
  ex() %>% 
    test_fun(name, index = index, not_called_msg = not_called_msg) %>% 
    test_result(error_msg = eval_error_msg) %>%
    test_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg)
}
