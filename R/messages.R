
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
