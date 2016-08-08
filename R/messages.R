trim <- function(x) gsub("^\\s+|\\s+$", "", x)

capitalize <- function(x) {
  x <- strsplit(x, split = "\\.\\s")[[1]]
  x <- paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = ". ")
  x <- strsplit(x, split = "\\?\\s")[[1]]
  x <- paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = "? ")
  return(x)
}

`%+=%` <- function(a, b) {
  eval.parent(substitute(a <- paste(a, b)))
}

build_feedback <- function(details) {
  msg <- ""

  for (det in details) {
    if (det$type == "object") {
      if (det$case == "defined") {
        msg %+=% sprintf("Did you define the variable `%s` without errors?", det$name)
      }
      if (det$case == "equal") {
        msg %+=% sprintf("The contents of the variable `%s` aren't correct.", det$name)
        msg %+=% build_diff(sol = det$solution, stud = det$student,
                            eq_condition = det$eq_condition,
                            id = sprintf("`%s`", det$name))
      }
    }
    if (det$type == "function") {
      if (det$case == "called") {
        msg %+=% sprintf("The system wants to check the %s call of `%s()`, but it hasn't found it.",
                         get_ord(det$index), det$name)
      }
      if (det$case == "correct") {
        msg %+=% sprintf("Check your call of `%s()`.", det$name)
      }
    }
    if (det$type == "argument") {
      if (det$case == "specified") {
        if (det$name == "...") {
          msg %+=% sprintf("Did you specify any arguments that are matched to `...`?", det$name)
        } else {
          msg %+=% sprintf("Did you specify the argument `%s`?", det$name)
        }
      }
      if (det$case == "equal") {
        if (det$name == "...") {
          msg %+=% "Did you correctly specify the arguments that are matched to `...`?"
        } else {
          msg %+=% sprintf("Did you correctly specify the argument `%s`?", det$name)
          msg %+=% build_diff(sol = det$solution, stud = det$student,
                              eq_condition = det$eq_condition,
                              id = "the object you specified")
        }
      }
    }
    if (det$type == "ifelse") {
      if (det$case == "defined") {
        msg %+=% sprintf("Are you sure you coded %s if statement%s?", get_num(det$index), ifelse(det$index > 1, "s", ""))  
      }
    }
    if (det$type == "ifcondition") {
      msg %+=% sprintf("Check the condition of the %s if statement.", get_ord(det$index))
    }
    if (det$type == "ifexpression") {
      msg %+=% sprintf("Check the body of the %s if statement.", get_ord(det$index))
    }
    if (det$type == "elseexpression") {
      if (det$case == "defined") {
        msg %+=% sprintf("The else part of the %s if statement is missing.", get_ord(det$index))
      } else if (det$case == "correct") {
        msg %+=% sprintf("Check the else part of the %s if statement.", get_ord(det$index))
      }
    }
    if (det$type == "typed") {
      msg %+=% sprintf("The system wanted to find the pattern `%s` %s but didn't.", det$regex, get_times(det$times))
    }
  }
  return(capitalize(trim(msg)))
}

build_summary <- function(x, ...) UseMethod("build_summary")

build_summary.default <- function(x) {
  toString(x, width = 300)
}

build_summary.list <- function(x) {
  # Back up names, recursion will mess them up otherwise
  tmp_names <- names(x)
  # Need to manually index using seq_along, doesn't work with element-wise lapply.
  x <- lapply(seq_along(x), function(i) { 
    build_summary(x[[i]]) 
  })
  if (!is.null(tmp_names)) {
    x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
  }
  trunc_str(x,"list")
}

build_summary.data.frame <- function(x) {
  # Back up names, recursion will mess them up otherwise
  tmp_names <- names(x)
  # Need to manually index using seq_along, doesn't work with element-wise lapply.
  x <- lapply(seq_along(x), function(i) { 
    build_summary(x[[i]]) 
  })
  if (!is.null(tmp_names)) {
    x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
  }
  trunc_str(x,"data.frame")
}

build_summary.character <- function(x, ..., output = FALSE) {
  if (output) {
    shorten <- function(str) { 
      paste0(substr(str, 1, 100), ifelse(nchar(str) > 100, "...", "")) 
    }
  } else {
    shorten <- function(str) { 
      paste0('"',substr(str, 1, 100), ifelse(nchar(str) > 100, "...", ""),'"') 
    }
  }
  if (length(x) > 1) {
    x <- lapply(x, shorten)
    trunc_str(x)
  } else {
    shorten(x)
  }
}

build_summary.numeric <- function(x) {
  if (length(x) > 1) {
    trunc_str(x)
  } else {
    x
  }
}

build_summary.factor <- function(x) {
  paste0("factor(",build_summary.character(as.character(x)),")")
}

test_summary <- function(x,...) {
  build_summary(x,...)
}
