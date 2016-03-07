# TEST_FUNCTION
build_arg_text <- function(n_args, args) {
  ifelse(n_args == 0, "", sprintf(" with %s %s", if (n_args == 1) "argument" else "arguments", collapse_args(args)))
}

# TODO: REVIEW LANGUAGE
build_function_call_text <- function(index) {
  sprintf(", in the %s call of the function", ifelse(get_language() == "en", get_num(index), index))
}

build_additional_text <- function(index) {
  sprintf(" in command %i of your solution", index)
}

# TODO: REVIEW LANGUAGE
build_not_enough_calls_text <- function(name, index) {
  sprintf("You are missing the %s call of <code>%s()</code>.", get_num(index), name)
}

build_not_called_msg <- function(n_solution_calls, name, arg_text, additionaltext) {
  if (n_solution_calls <= 1) n_text <- ""
  else if (n_solution_calls == 2) n_text <- " twice"
  else n_text <- sprintf(" %d times", n_solution_calls)
  sprintf("Did you call function <code>%s()</code>%s%s%s?",
          name, n_text, arg_text, additionaltext)  
}

build_incorrect_msg <- function(n_solution_calls, n_args, arg_text, name, additionaltext) {
  lang <- get_language()
  insert <- if (n_solution_calls == 1) "" else " always"
  val_text <- if (n_args == 1) "value" else "values"
  arg_text <- gsub(" with", "for", arg_text)  # whitespace is important
  sprintf("It looks like you didn't%s set the correct %s %s in function <code>%s()</code>%s.",
          insert, val_text, arg_text, name, additionaltext)
}

# TEST_OBJECT
build_undefined_object_msg <- function(name) {
  sprintf("Did you define <code>%s</code>?", name)
}

build_incorrect_object_msg <- function(name) {
  sprintf("It looks like you didn't assign the correct value to <code>%s</code>.", name)
}

# TEST_OUTPUT_CONTAINS
build_incorrect_output_msg <- function(expr) {
  sprintf("Make sure to print <code>%s</code> to the console", expr)
}


build_summary <- function(x, ...) UseMethod("build_summary")

build_summary.default <- function(x) {
  toString(x, width = 300)
}

build_summary.list <- function(x) {
  # Back up names, recursion will mess them up otherwise
  tmp_names <- names(x)
  # Need to manually index using seq_along, doesn't work with element-wise lapply.
  x <- lapply(seq_along(x), function(i) { build_summary(x[[i]]) })
  if (!is.null(tmp_names)) {
    x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
  }
  trunc_str(x,"list")
}

build_summary.data.frame <- function(x) {
  # Back up names, recursion will mess them up otherwise
  tmp_names <- names(x)
  # Need to manually index using seq_along, doesn't work with element-wise lapply.
  x <- lapply(seq_along(x), function(i) { build_summary(x[[i]]) })
  if (!is.null(tmp_names)) {
    x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
  }
  trunc_str(x,"data.frame")
}

build_summary.character <- function(x, ..., output = FALSE) {
  if (output) {
    shorten <- function(str) { paste0(substr(str, 1, 100), ifelse(nchar(str) > 100, "...", "")) }
  } else {
    shorten <- function(str) { paste0('"',substr(str, 1, 100), ifelse(nchar(str) > 100, "...", ""),'"') }
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
