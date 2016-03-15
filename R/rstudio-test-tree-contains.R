#' Test whether a student's function call contains a certain character (dplyr and ggvis exercises)
#'
#' For a specified command in the student's code, check whether a particular function contains
#' a set of queries. Also the number of times these queries have to appear can be specified.
#'
#' This test is implemented using \code{\link{test_that}}. Only exact string matching is
#' performed for the moment.
#'
#' @param index  exercise to be checked (solution and student code should have same number of calls!)
#' @param fun  name of the function to be checked. if fun = NULL, check the entire command.
#' @param queries  single character or vector of character that have to be present in the function
#' @param times  number of times each of the entered queries have to be available. (ones on default)
#' @param contain_all  AND versus OR. if contain_all = TRUE (by default), all strings in the queries
#' vector must exist in the tree. If contain_all = FALSE, the test passes if one of the strings in the
#' queries vector exists in the tree the specified number of times.
#' @param fixed_order whether or not the queries have to appear in the function call the same order
#' as specified in the queries vector (default FALSE).
#' This functionality can not be used when contain_all is FALSE.
#' This option also only works when each query has to be present only once.
#' The order is determined on the LAST occurrences of the queries (in embedded notation!!)
#' @param not_called_msg feedback message in case the function is not retrieved
#' @param absent_msg  feedback message in case one of the queries was not available
#' @param incorrect_number_of_calls_msg  feedback message in case the student did
#' enter the same amount of commands as the solution did.
#'
#' @export
test_tree_contains <- function(index = 1, fun = NULL, queries = NULL,
                               times = NULL, contain_all = TRUE, fixed_order = FALSE,
                               not_called_msg = NULL,
                               absent_msg = NULL,
                               incorrect_number_of_calls_msg = NULL) {
  
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  init_tags(fun = "test_tree_contains")
  
  if(is.null(queries)) {
    stop("argument 'queries' can not be empty")
  }
  if(is.null(times)) {
    times = rep(1,length(queries))
  }
  if(length(times) != length(queries)) {
    stop("times and queries do not have the same length!")
  }
  
  # If necessary, build parse calls for both student code and solution code.
  pd_stud <- get_single_pd(index = index, pd = create_student_pd(student_code = student_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  if(is.null(pd_stud)) {
    return(FALSE)
  }
  
  funstr <- if(is.null(fun)) "the entire command" else sprintf("<code>%s()</code>",fun)
  
  expressions = get_expressions_for_function_call(fun = fun, pd = pd_stud)
  
  if(is.null(not_called_msg)) {
    not_called_msg = sprintf("Function <code>%s()</code> was not called in command %i of your solution.", fun, index);
  }
  
  if (!is.null(fun)) {
    test_what(expect_true(length(expressions) > 0), feedback_msg = not_called_msg)
  }
  
  result = sapply(expressions, expression_contains, queries, times, contain_all, fixed_order)
  
  # set up default absent_msg if not present
  if(is.null(absent_msg)) {
    if(is.null(fun)) funtext <- ""
    else funtext <- sprintf(" inside %s", funstr)
    if(contain_all && fixed_order) {
      absent_msg = sprintf("In command %i of your solution, make sure to use %s%s the correct number of times and in the correct order!",
                           index, testwhat:::collapse_props(queries), funtext)
    }
    else if(contain_all && !fixed_order) {
      absent_msg = sprintf("In command %i of your solution, make sure to use %s%s the correct number of times!",
                           index, testwhat:::collapse_props(queries), funtext)
    } else {
      absent_msg = sprintf("In command %i of your solution, %s were not found%s sufficiently. Make sure to use at least one of them!",
                           index, testwhat:::collapse_props(queries), funtext)
    }
  }
  test_what(expect_true(any(result)), feedback_msg = absent_msg)
}

expression_contains = function(expression, queries, times, contain_all, fixed_order) {
  smallpd = getParseData(parse(text = expression, keep.source = TRUE))
  freqs = sapply(queries, function(x) length(smallpd$id[are_equal(smallpd$text,x)]))
  if(contain_all) {
    # check if ALL queries are found equally frequent or more frequent
    diff = freqs - times
    pass = all(freqs - times >= 0)
    if(pass & fixed_order) {
      locs = sapply(queries, function(x) tail(smallpd$id[smallpd$text == x],n=1))
      pass = all.equal(sort(locs), locs)
      if(is.character(pass))
        pass = FALSE
    }
  } else {
    pass = any(freqs - times >= 0)
  }
  return(pass)
}

are_equal = function(a,b){
  return(gsub("'","\"",a) == gsub("'","\"",b))
}
