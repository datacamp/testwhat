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
  
has_arguments <- function(call, args, ignore = NULL, allow_extra = TRUE) {
  if (allow_extra) 
    all(args %in% names(call)[-1])
  else {
    supplied <- setdiff(names(call)[-1], ignore)
    is_equal(args, supplied)
  }
}

# Extract specified arguments from a function call and evaluate if necessary
extract_arguments <- function(call, args, eval, env) {
  mapply(function(arg, eval) {
    object <- call[[arg]]
    if (eval && (is.name(object) || is.call(object) || is.expression(object))) {
      object <- try(eval(object, envir = env), silent = TRUE)
      if (inherits(object, "try-error")) {
        object <- tryerrorstring
      }
    }
    object
  }, args, eval, SIMPLIFY = FALSE)
}

tryerrorstring <- "try-error-in-test-function"
