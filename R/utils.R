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

failure <- function() {
  FALSE
}

get_solution_code <- function() { ex()$get("solution_code") }

#' @importFrom magrittr %>%
NULL

#' @importFrom testwhat.base check_that test_what is_equal is_false is_true is_gte
NULL

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

assert_is_string <- function(x, sct_name) {
  if (!is.character(x))
    stop(x, paste0(sys.call(1)[1], " requires a string, but received the class", typeof(x), '.'))
}
