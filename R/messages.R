trim <- function(x) {
  x <- gsub("^\\s+|\\s+$", "", x)
  x <- gsub("  ", " ", x) # This is dangerous, watch out.
}

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

`%+=0%` <- function(a, b) {
  eval.parent(substitute(a <- paste0(a, b)))
}

build_feedback_message <- function(details) {
  msg <- ""

  for (det in details) {
    if (!is.null(det$message)) {
      msg %+=% det$message
      next
    }
    if (det$type == "object") {
      if (det$case == "defined") {
        msg %+=% sprintf("Did you define the variable `%s` without errors?", det$name)
      } 
      if (det$case == "correct") {
        msg %+=% sprintf("The contents of the variable `%s` aren't correct.", det$name)
      }
      if (det$case == "equal") {
        msg %+=% build_diff(sol = det$solution, stud = det$student,
                            eq_condition = det$eq_condition,
                            id = "it")
      }
    }
    if (det$type == "function") {
      if (det$case == "called") {
        msg %+=% sprintf("Have you called `%s()`%s?",
                         det$name, ifelse(det$index == 1, "", paste0(" ", get_times(det$index))))
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
      if (det$case == "correct") {
        if (det$name == "...") {
          msg %+=% "Did you correctly specify the arguments that are matched to `...`?"
        } else {
          msg %+=% sprintf("Did you correctly specify the argument `%s`?", det$name)
        }
      }
      if (det$case == "equal") {
        if (!det$is_dots) {
          msg %+=% build_diff(sol = det$solution, stud = det$student,
                              eq_condition = det$eq_condition,
                              id = "it")  
        }
      }
    }
    if (det$type %in% c("if", "for", "while")) {
      if (det$case == "defined") {
        msg %+=% sprintf("Are you sure you coded %s %s statement%s?", get_num(det$index), det$type, ifelse(det$index > 1, "s", ""))  
      } else if (det$case == "correct") {
        msg %+=% sprintf("Check the %s %s statement.", get_ord(det$index), det$type)
      }
    }
    if (det$type == "condition") {
      msg %+=% sprintf("Check the condition.")
    }
    if (det$type == "body") {
      msg %+=% sprintf("Check the body.")
    }
    if (det$type == "ifexpression") {
      msg %+=% sprintf("Check the if part.")
    }
    if (det$type == "elseexpression") {
      if (det$case == "defined") {
        msg %+=% sprintf("The else part is missing.")
      } else if (det$case == "correct") {
        msg %+=% sprintf("Check the else part.")
      }
    }
    if (det$type == "typed") {
      msg %+=% sprintf("The system wanted to find the pattern `%s` %s but didn't.", det$regex, get_times(det$times))
    }
    if (det$type == "fundef") {
      if (det$case == "defined") {
        msg %+=% sprintf("Did you define the function <code>%s()</code>?", det$name)
      }
      if (det$case == "correcttype") {
        msg %+=% sprintf("Are you sure that <code>%s</code> is a function?", det$name)
      }
      if (det$case == "correct") {
        msg %+=% sprintf("Did you correctly define the function <code>%s()</code>?", det$name)
      }
      if (det$case == "arguments") {
        msg %+=% "Did you specify the correct number of arguments?"
      }
      if (det$case == "coded") {
        msg %+=% sprintf("The system couldn't the definition in your code")
      }
      if (det$case == "result_runs") {
        msg %+=% sprintf("Running %s generated an error.", det$callstr)
      }
      if (det$case == "result_correct") {
        msg %+=% sprintf("Calling %s didn't give the correct result.", det$callstr)
      }
      if (det$case == "result_equal") {
        msg %+=% build_diff(sol = det$solution, stud = det$student,
                            eq_condition = det$eq_condition,
                            id = "the result")
      }
      if (det$case == "output_runs") {
        msg %+=% sprintf("Running %s generated an error.", det$callstr)
      }
      if (det$case == "output_correct") {
        msg %+=% sprintf("Calling %s didn't generate the correct output.", det$callstr)
      }
      if (det$case == "output_equal") {
        msg %+=% sprintf("Expected %s, but got %s",
                         ifelse(length(det$solution) == 0, "no output", sprintf("`%s`", det$solution)),
                         ifelse(length(det$student) == 0, "no output", sprintf("`%s`", det$student)))
      }
      if (det$case == "error_fails") {
        msg %+=% sprintf("Running %s didn't generate an error, but it should.", det$callstr)
      }
      if (det$case == "error_correct") {
        msg %+=% sprintf("Calling %s didn't generate the correct error.", det$callstr)
      }
      if (det$case == "error_equal") {
        msg %+=% sprintf("Expected the error `%s`, but instead got the error `%s`",
                         det$solution, det$student)
      }
    }
  }
  return(capitalize(trim(msg)))
}

# check_that(is_false(is.null(student_fun_def)), 
#            feedback = sprintf("A proper definition of `%s` could not be found in your submission. Make sure to use the `%s <- function() { ... }` recipe.", name, name))

# if (is.null(undefined_msg)) {
#   undefined_msg <- sprintf("Did you define the function <code>%s()</code>?", name)
# }
# 
# if (is.null(incorrect_number_arguments_msg)) {
#   incorrect_number_arguments_msg <- sprintf()
# }
# 

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
