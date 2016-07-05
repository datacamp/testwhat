sct_failed_msg <- "<sct_failed_error>"

check_defined <- function(name, sol_env) {
  if (!exists(name, sol_env, inherits = FALSE)) {
    stop(paste(name, "is not defined in your solution environment.",
               "Specify the name of an object that is actually defined in the solution code"))
  }
}

check_sufficient <- function(calls, index, name) {
  if (index > length(calls)) {
    stop(sprintf("Fix either the index argument or the solution code; currently, there aren't %s calls of %s() available in the solution.", index, name))
  }
}
invalid_eq_condition <- "eq_condition should be either 'equivalent', 'equal' or 'identical'."

# Check equality with a specified equality condition
is_equal <- function(x, y, condition = "equivalent") {
  eq_fun <- switch(condition, equivalent = .equivalent, equal = .equal,
                   identical = identical, stop("invalid equality condition"))
  eq_fun(x, y)
}

.equivalent <- function(x, y) compare(x, y, check.attributes = FALSE)$equal
.equal <- function(x, y) compare(x, y)$equal

#' Get solution environment (backwards compatbility)
#' @export
get_solution_env <- function() { tw$get("solution_env") }

#' Get solution environment (backwards comp)
#' @export
get_student_code <- function() { tw$get("student_code") }

#' Get solution environment (backwards comp)
#' @export
get_solution_code <- function() { tw$get("solution_code") }

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

init_tags <- function(...) {
  tw$set(tags = list(...))
}

set_tags <- function(...) {
  tw$set(tags = merge_list(tw$get("tags"), list(...)))
}
