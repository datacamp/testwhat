sct_failed_msg <- "<sct_failed_error>"

check_defined <- function(name, sol_env) {
  if (!exists(name, sol_env, inherits = FALSE)) {
    stop(paste(name, "is not defined in your solution environment.",
               "Specify the name of an object that is actually defined in the solution code"))
  }
}

check_sufficient <- function(calls, index, name) {
  if (index > length(calls)) {
    stop(sprintf("Fix either the index argument or the solution code; currently, there aren't %s calls of %s available in the solution.", index, name))
  }
}

#' @importFrom magrittr %>%
#'
#' @export
magrittr::`%>%`

#' Execute code in the student environment
#' @param code The code to execute
#' 
#' @export
execute_student <- function(code) {
  try(eval(substitute(code), envir = get_student_env()))
}

#' Execute code in the solution environment
#' @param code The code to execute
#'
#' @export
execute_solution <- function(code) {
  try(eval(substitute(code), envir = get_solution_env()))
}

#' Get solution environment (backwards comp)
#' @export
get_solution_env <- function() { ex()$get("solution_env") }

#' Get student environment (backwards comp)
#' @export
get_student_env <- function() { ex()$get("student_env") }

#' Get solution environment (backwards comp)
#' @export
get_student_code <- function() { ex()$get("student_code") }

#' Get solution environment (backwards comp)
#' @export
get_solution_code <- function() { ex()$get("solution_code") }

tw_accessors <- function() {
  tw_data <- list()
  
  get = function(name) {
    if(missing(name)) {
      tw_data
    } else {
      tw_data[[name]]
    }
  }
  
  set = function(...) {
    tw_data <<- merge(list(...))
    invisible(NULL)
  }
  
  clear = function() {
    tw_data <<- list()
    invisible(NULL)
  }
  
  initialize = function(data) {
    tw_data <<- data
    invisible(NULL)
  }
  
  merge = function(values) merge_list(tw_data, values)
  list(get = get, set = set, clear = clear, initialize = initialize)
}

merge_list <- function(x, y) {
  x[names(y)] = y
  x
}

tw <- tw_accessors()

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

assert_is_string <- function(x, sct_name) {
  if (!is.character(x))
    stop(x, paste0(sys.call(1)[1], " requires a string, but received the class", typeof(x), '.'))
}
