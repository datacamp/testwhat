#' Test whether a student correctly called a function
#'
#' Test whether a student called a function with certain arguments as least as
#' many times as in a sample solution.  If yes, test for each function call in
#' the sample solution whether the student called the function with the same
#' argument values.
#'
#' This test is implemented using \code{\link{test_that}}.  When testing
#' whether argument values are the same, small numeric differences or
#' differences in attributes are allowed.
#'
#' See vignette \code{"testwhat-intro"} for examples.
#'
#' @param name  name of the function to test.
#' @param args  character vector of argument names that the student should have
#' supplied in the function calls.  If no argument names are given, it is only
#' tested whether the student called the function at least as many times as in
#' the sample solution.
#' @param ignore character vector of argument names that should not be tested
#' (useful in combination with \code{allow_extra = FALSE} to allow certain
#' arguments to be ignored, but not others).
#' @param allow_extra  indicates whether extra arguments not specified by
#' \code{args} or \code{ignore} are allowed in the student's function calls.
#' @param eval  logical vector indicating whether the corresponding argument
#' should be evaluated before testing.  Setting this to \code{FALSE} can be
#' useful, e.g., to test whether the student supplied a large predefined
#' object, as only the corresponding \code{\link{name}} is compared in this
#' case (use with care!).
#' @param index  Which command to check on (both in solution and student).
#' If index is specified, it is checked whether solution and student contain
#' the same amount of commands. index = NULL by default, i.e. the entire submission is checked.
#' @param eq_condition  character vector indicating how to perform the
#' comparison for each argument.  Possible values are \code{"equivalent"}
#' (the default), \code{"equal"} and \code{"identical"}.  See
#' \code{\link{is_equivalent_to}}, \code{\link{equals}}, and
#' \code{\link{is_identical_to}}, respectively.
#' @param student_code  character string containing the student's code.
#' @param solution_code  character string containing the sample solution code.
#' @param not_called_msg  feedback message in case the student did not call the
#' function at least as often as in the solution code.
#' @param incorrect_msg  feedback message in case the student did not call the
#' function with the same argument values as in the sample solution.  If
#' there are multiple function calls in the sample solution, a vector of
#' feedback messages can be supplied.
#' @param incorrect_number_of_calls_msg feedback message in case the number of commands
#' in the solution does not correspond to the solution. (only used if index is not NULL.)
#' @inheritParams test_object
#'
#' @examples
#' \dontrun{
#' # Suppose the solution contains: mean(1:3, na.rm = TRUE)
#' # To test this submission, provide the following in the sct
#' test_function("mean", c("x", "na.rm"))
#' }
#'
#' # Other examples: see SCT design guide
#'
#' @export
test_function <- function(name, args = NULL, ignore = NULL,
                          allow_extra = TRUE,
                          eval = TRUE,
                          index = NULL,
                          eq_condition = "equivalent",
                          student_code = get_student_code(),
                          solution_code = get_solution_code(),
                          student_env = .GlobalEnv,
                          solution_env = get_solution_env(),
                          not_called_msg = NULL, incorrect_msg = NULL,
                          incorrect_number_of_calls_msg = NULL) {
  n_args <- length(args)
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  
  arg_text <- build_arg_text(n_args, args)

  # remove the pipe operator from the calls, if present.
  student_code_parts = get_clean_lines(code = student_code)
  solution_code_parts = get_clean_lines(code = solution_code)
  
  if(is.null(index)) {
    # paste together again, all code is considered
    student_code <- paste0(student_code_parts, separator = "\n", collapse = "")
    solution_code <- paste0(solution_code_parts, separator = "\n", collapse = "")
    additionaltext <- ""
  } else {
    # check if equal number of commands and select parts of code
    ok = test_sufficient_length(student_code_parts, index, incorrect_number_of_calls_msg)
    if(isTRUE(ok)) {
      student_code = student_code_parts[index]
      solution_code = solution_code_parts[index]
      additionaltext <- build_additional_text(index)
    } else {
      return(FALSE)
    }
  }
  
  test_that(sprintf("Function %s()%s is correctly called", name, arg_text), {
    
    # Find all function calls in the student and solution code
    student_calls <- find_function_calls(name, student_code, student_env)
    solution_calls <- find_function_calls(name, solution_code, solution_env)
    if (n_args > 0) {
      # Only use function calls with the specified arguments
      keep_student <- have_arguments(student_calls, args, ignore, allow_extra)
      student_calls <- student_calls[keep_student]
      keep_solution <- have_arguments(solution_calls, args, ignore, allow_extra)
      solution_calls <- solution_calls[keep_solution]
    }
    n_student_calls <- length(student_calls)
    n_solution_calls <- length(solution_calls)
    
    # Test if there are at least as many student function calls as solution
    # function calls
    if (is.null(not_called_msg)) {
      not_called_msg <- build_not_called_msg(n_solution_calls, name, arg_text, additionaltext)
    }
    expect_that(n_student_calls >= n_solution_calls, is_true(),
                failure_msg = not_called_msg)
    
    ## If supplied, test arguments
    if (n_args > 0) {
      
      # Loop over the solution function calls:
      # Extract the specified arguments from current solution function call and
      # test if there exists a student function call with the same values
      if (is.null(incorrect_msg)) {
        incorrect_msg <- build_incorrect_msg(n_solution_calls, n_args, arg_text, name, additionaltext)
      }
      incorrect_msg <- rep(incorrect_msg, length.out = n_solution_calls)
      for (i in seq_len(n_solution_calls)) {
        # Extract the specified arguments from current solution function call
        solution_args <- extract_arguments(solution_calls[[i]], args, eval,
                                           env = solution_env)
        
        # Loop over the student function calls:
        # Extract the specified arguments from current student function call
        # and test if they are the same as the values in the solution
        for (j in seq_len(n_student_calls)) {
          student_args <- extract_arguments(student_calls[[j]], args, eval,
                                            env = student_env)
          correct <- mapply(is_equal, student_args, solution_args, eq_condition)
          correct <- all(correct)
          
          if (correct) {
            student_calls[[j]] <- NULL
            n_student_calls <- length(student_calls)
            break
          }
        }
        expect_that(correct, is_true(), failure_msg = incorrect_msg[[i]])
      }
    }
  })
}

# Extract specified arguments from a function call and evaluate if necessary
extract_arguments <- function(call, args, eval = TRUE, env = parent.frame()) {
  mapply(function(arg, eval) {
    object <- call[[arg]]
    if (eval && (is.name(object) || is.call(object) || is.expression(object))) {
      object <- eval(object, envir = env)
    }
    object
  }, args, eval, SIMPLIFY = FALSE)
}

# Find all calls to a given function within a piece of code
find_function_calls <- function(name, code, env = parent.frame()) {
  # Parse user code and get parse information (keep.source = TRUE
  # is important, otherwise it doesn't work within knitr)
  parseData <- getParseData(parse(text = code, keep.source = TRUE))

  # Retrieve all function calls from parse information
  called <- parseData$text == name & parseData$token == "SYMBOL_FUNCTION_CALL"
  fun_ids <- parseData$parent[called]
  expr_ids <- parseData$parent[parseData$id %in% fun_ids]
  expr_strings <- getParseText(parseData, expr_ids)
  exprs <- parse(text = expr_strings)

  # Expand arguments of function calls
  lapply(exprs, standardize_call, env = env)
}

# Check if function calls have (only) the specified arguments
have_arguments <- function(calls, args, ignore = NULL, allow_extra = TRUE) {
  if (allow_extra) fun <- function(call) all(args %in% names(call)[-1])
  else {
    fun <- function(call) {
      supplied <- setdiff(names(call)[-1], ignore)
      compare(args, supplied)$equal
    }
  }
  vapply(calls, fun, logical(1))
}

# Check equality with a specified equality condition
is_equal <- function(x, y, condition = "equivalent") {
  eq_fun <- switch(condition, equivalent = .equivalent, equal = .equal,
                   identical = identical, stop("invalid equality condition"))
  eq_fun(x, y)
}

.equivalent <- function(x, y) compare(x, y, check.attributes = FALSE)$equal
.equal <- function(x, y) compare(x, y)$equal

# Expand argument names of a function call
standardize_call <- function (call, env = parent.frame()) {
  stopifnot(is.call(call))
  
  f <- args(eval(call[[1]], env))
  
  match.call(f, call)
}
