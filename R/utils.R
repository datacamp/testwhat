#' Get the number of hits for a series of regexes
#'
#' @param regex vector of regular expressions against which to match
#' @param x character vector where matches are sought
#' @param fixed logical. If \code{TRUE}, \code{regex} are strings to be matched as is.
#'
#' @export
get_num_hits <- function(regex, x, fixed) {
  if (length(regex) == 0 || (length(regex) == 1 && nchar(regex) == 0)) {
    return(0)
  } else {
    counts <- sapply(regex, function(patt) {
      res <- gregexpr(patt, text = x, fixed = fixed)[[1]]
      if (any(res == -1)) {
        return(0L)
      } else {
        return(length(res))
      }
    }, USE.NAMES = FALSE)
    if (length(counts) == 0) {
      return(0)
    } else {
      return(sum(counts))
    }
  }
}

remove_comments <- function(code) {
  lines <- strsplit(code, "\\n")[[1]]
  return(paste0(lines[!grepl("^#", lines)], collapse = "\n"))
}

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

fail_if_v2_only <- function(errmsg = sprintf("%s() can no longer be used in SCTs. Use its check equivalent instead.", deparse(sys.call(-1)[[1]]))) {
  env_var <- "TESTWHAT_V2_ONLY"
  if (Sys.getenv(env_var) != "" && Sys.getenv(env_var) == "1") {
    stop(errmsg)
  } else {
    return(invisible(NULL))
  }
}

failure <- function() {
  FALSE
}

get_solution_code <- function() { ex()$get("solution_code") }

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

assert_is_string <- function(x, sct_name) {
  if (!is.character(x))
    stop(x, paste0(sys.call(1)[1], " requires a string, but received the class", typeof(x), '.'))
}

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

#' tw singleton object to access data across SCT chains.
#'
#' @export
tw <- tw_accessors()

