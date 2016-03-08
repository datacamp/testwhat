#' Test whether a student correctly called a function 
#'
#' Test whether a student called a function, possibly with certain arguments, correctly.
#' 
#' @param name  name of the function to test.
#' @param args  character vector of argument names that the student should have
#' supplied in the function calls.
#' @param index  integer that specifies which call of \code{name} in the solution 
#' code will be checked.
#' @param ignore character vector of argument names that should not be tested
#' (useful in combination with \code{allow_extra = FALSE} to allow certain
#' arguments to be ignored, but not others).
#' @param allow_extra  indicates whether extra arguments not specified by
#' \code{args} or \code{ignore} are allowed in the student's function calls.
#' @param eval  logical vector indicating whether the corresponding argument
#' should be evaluated before testing. Setting this to \code{FALSE} can be
#' useful, e.g., to test whether the student supplied a large predefined
#' object, as only the corresponding \code{\link{name}} is compared in this
#' case (use with care!).
#' @param eq_condition  character vector indicating how to perform the
#' comparison for each argument. See \code{\link{test_object}}
#' @param not_called_msg feedback message in case the student did not call the
#' function often enough.
#' @param args_not_specified_msg feedback message in case the student did call the function
#' with the arguments listed in \code{args}
#' @param incorrect_msg  feedback message in case the student did not call the
#' function with the same argument values as in the sample solution.  If
#' there are multiple function calls in the sample solution, a vector of
#' feedback messages can be supplied.
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
                          ignore = NULL,
                          allow_extra = TRUE,
                          eval = TRUE,
                          eq_condition = "equivalent",
                          not_called_msg = NULL, 
                          args_not_specified_msg = NULL,
                          incorrect_msg = NULL) {
  
  student_env <- tw$get("student_env")
  solution_env <- tw$get("solution_env")
  student_pd <- tw$get("student_pd")
  solution_pd <- tw$get("solution_pd")
  init_tags(fun = "test_function")
  
  n_args <- length(args)
  eval <- rep(eval, length.out = n_args)
  eq_condition <- rep(eq_condition, length.out = n_args)
  
  # Find all function calls in the student and solution code
  student_calls <- find_function_calls(name, student_pd, student_env)
  solution_calls <- find_function_calls(name, solution_pd, solution_env)
  n_student_calls <- length(student_calls)
  n_solution_calls <- length(solution_calls)

  # Check if index exists in solution
  if(index > length(solution_calls)) {
    stop(sprintf("There aren't %s calls of `%s()` available in the solution.", index, name))
  }
  solution_call <- solution_calls[[index]]
  
  if(is.null(not_called_msg)) {
    sprintf("You are missing the %s call of `%s()`.", get_num(index), name)
  }
  test_what(expect_true(n_student_calls >= index), list(message = not_called_msg))
  
  if (n_args > 0) {
    args_specified_passed <- FALSE
    args_correct_passed <- FALSE
    args_specified_feedback <- NULL
    args_correct_feedback <- NULL
    
    if(!has_arguments(solution_call$call, args, ignore, allow_extra)) {
      stop("The solution call doesn't have the listed arguments itself.")  
    }
    
    # iterate over all student calls, except the ones that are blacklisted
    seq <- setdiff(1:n_student_calls, get_blacklist(name))
    for(i in seq) {
      student_call <- student_calls[[i]]
      
      # Check if the function is called with the right arguments
      args_specified <- has_arguments(student_call$call, args, ignore, allow_extra)
      if(!args_specified) {
        if(is.null(args_specified_feedback)) {
          if(is.null(args_not_specified_msg)) {
            args_not_specified_msg <- sprintf("Did you specify the argument%s %s in your call of `%s()`?", 
                                              ifelse(n_args > 1, "s", ""), collapse_args(args), name)
          }
          args_specified_feedback <- list(message = args_not_specified_msg,
                                          line_start = student_call$line1,
                                          line_end = student_call$line2,
                                          column_start = student_call$col1,
                                          column_end = student_call$col2)  
        }
        next
      } else {
        args_specified_passed <- TRUE
      }
      
      # Test if the specified arguments are correctly called
      student_args <- extract_arguments(student_call$call, args, eval, env = student_env)
      solution_args <- extract_arguments(solution_call$call, args, eval, env = solution_env)
      
      args_correct_vec <- mapply(is_equal, student_args, solution_args, eq_condition)
      args_correct <- all(args_correct_vec)
      if(!args_correct) {
        if(is.null(args_correct_feedback)) {
          if(is.null(incorrect_msg)) {
            incorrect_args <- args[!args_correct_vec]
            incorrect_msg <- sprintf("Did you correctly specify the argument%s %s in your call of `%s()`?", 
                                     ifelse(length(incorrect_args) > 1, "s", ""), collapse_args(incorrect_args), name)
          }
          args_correct_feedback <- list(message = incorrect_msg,
                                        line_start = student_call$line1,
                                        line_end = student_call$line2,
                                        column_start = student_call$col1,
                                        column_end = student_call$col2)
        }
        next
      } else {
        args_correct_passed <- TRUE
        # we have a winner. Blacklist this student call!
        blacklist(name, index = i)
        break
      }
    }
    
    if(!args_correct_passed) {
      # Still need something that fails...
      if(!args_specified_passed) {
        test_what(fail(), args_specified_feedback)
      } else {
        test_what(fail(), args_correct_feedback)
      }
    }
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
    compare(args, supplied)$equal
  }
}

# Extract specified arguments from a function call and evaluate if necessary
extract_arguments <- function(call, args, eval = TRUE, env = parent.frame()) {
  mapply(function(arg, eval) {
    object <- call[[arg]]
    if (eval && (is.name(object) || is.call(object) || is.expression(object))) {
      object <- try(eval(object, envir = env), silent = TRUE)
      if (inherits(object, "try-error")) {
        return(NULL)
      }
    }
    object
  }, args, eval, SIMPLIFY = FALSE)
}

# Find all calls to a given function within a piece of code
find_function_calls <- function(name, pd, env = parent.frame()) {
  
  # Retrieve all function calls from parse information
  called <- pd$text == name & pd$token == "SYMBOL_FUNCTION_CALL"
  fun_ids <- pd$parent[called]
  expr_ids <- pd$parent[pd$id %in% fun_ids]
  expr_strings <- getParseText(pd, expr_ids)
  exprs <- parse(text = expr_strings)
  
  # Expand arguments of function calls
  mapply(function(expr,expr_string,expr_id) {
    call <- standardize_call(expr,expr_string,env)
    c(list(call = call), as.list(pd[pd$id == expr_id, c("line1", "col1", "line2", "col2")]))
  }, exprs, expr_strings, expr_ids, SIMPLIFY = FALSE)
}

# Check equality with a specified equality condition
is_equal <- function(x, y, condition = "equivalent") {
  eq_fun <- switch(condition, equivalent = .equivalent, equal = .equal,
                   identical = identical, stop("invalid equality condition"))
  eq_fun(x, y)
}

.equivalent <- function(x, y) compare(x, y, check.attributes = FALSE)$equal
.equal <- function(x, y) compare(x, y)$equal
#' @importFrom stringdist stringdist
.like <- function(x, y, dist = round(nchar(y) * 0.2)) stringdist(x,y) <= dist

# Expand argument names of a function call
standardize_call <- function (call, call_string, env = parent.frame()) {
  stopifnot(is.call(call))
  
  f <- args(get(as.character(call[[1]]), env))
  
  e <- try(match.call(f, call), silent = TRUE)
  
  e <- find_S3_call(e, env = env)
  
  if (inherits(e, "try-error")) {
    test_what(fail(), 
              sprintf("There is something wrong in the following function call **%s**: _%s_", 
                      call_string,
                      attr(e,"condition")$message))
  } else {
    return(e)
  }
}

find_S3_call <- function (matched_call, env = parent.frame()) {
  if (inherits(matched_call, "try-error")) {
    return(matched_call)
  }
  call_method <- as.character(matched_call[[1]])
  met <- try(methods(call_method), silent = TRUE)
  if (inherits(met, "try-error")) {
    return(matched_call)
  } else if (length(met) == 0) {
    return(matched_call)
  } else if (length(matched_call) < 2) {
    return(matched_call)
  } else {
    call_class <- try(class(eval(matched_call[[2]], env)), silent = TRUE)
    if (inherits(call_class, "try-error")) {
      return(matched_call)
    }
    call_dispatched <- paste(call_method,call_class, sep = ".")
    find_call <- rep(FALSE, length(met))
    for (one_call in call_dispatched) {
      find_call <- met==one_call
      if (any(find_call)) {
        call_dispatched <- one_call
        break
      }
    }
    if (!any(find_call)) {
      call_dispatched <- paste(call_method, "default", sep = ".")
      cal_class <- "default"
      find_call <- met==call_dispatched
      if (!any(find_call)) {
        # At this point, we are almost certain the call is a primitive.
        # Just ignore.
        return(matched_call)
      }
    }
    find_call <- which(find_call)
    vis <- attr(met, "info")$visible[find_call]
    if (vis) {
      f <- args(get(call_dispatched, env))
    } else {
      f <- args(getAnywhere(call_dispatched)[1])
    }
    # Nothing is done with this structure yet
    return(try(match.call(f, matched_call), silent = TRUE))
#     return(structure(try(match.call(f, matched_call), silent = TRUE),
#                      s3_class = call_dispatched,
#                      s3_arg_name = as.character(names(matched_call)[[2]])))
  }
}

blacklist <- function(name, index) {
  bl <- tw$get("blacklist")
  if(is.null(bl) || is.null(bl[[name]])) {
    # no blacklist yet or none for function yet
    tw$set(blacklist = c(bl, structure(list(index), names = name)))
  } else {
    # blacklist available, and previous calls blacklisted for function
    bl[[name]] <- append(bl[[name]], index)
    tw$set(blacklist = bl)
  }
}

get_blacklist <- function(name) {
  bl <- tw$get("blacklist")
  if(is.null(bl)) {
    return(NULL)
  } else {
    return(bl[[name]])
  }
}