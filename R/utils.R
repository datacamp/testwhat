#' Get solution environment (backwards comp)
#' @export
get_solution_env <- function() { tw$get("solution_env") }

#' Get solution environment (backwards comp)
#' @export
get_student_code <- function() { tw$get("student_code") }

#' Get solution environment (backwards comp)
#' @export
get_solution_code <- function() { tw$get("solution_code") }

#' Get solution environment (backwards comp)
#' @export
get_student_output <- function() { get(DM.console.output, envir = globalenv()) }

sct_failed_msg <- "<sct_failed_error>"

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

# Find expression that created a variable
find_expr <- function(name, env = parent.frame()) {
  subs <- do.call("substitute", list(as.name(name), env))
  paste0(deparse(subs, width.cutoff = 500), collapse = "\n")
}
