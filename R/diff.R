build_diff <- function(...) {
  res <- get_diff(...)
  if (is.null(res)) {
    return(NULL)
  } else {
    # capitalize + space in front
    return(paste0(" ", toupper(substring(res, 1, 1)), substring(res, 2)))
  }
}

get_diff <- function(x, y, ...) {
  UseMethod("get_diff", x)
}

get_diff.default <- function(x, ...) {
  return(NULL)
}

## Class-specific diffs

get_diff.logical <- function(x, y, eq_condition, id) {
  if (!typeof(y) %in% c("logical", "integer", "double")) {
    return(diff_type(x, y, id))
  }
  if (!same_class(x, y)) {
    return(diff_class(x, y, id))
  }
  if (!same_length(x, y)) {
    return(diff_length(x, y, id))
  }
  if (check_attr(eq_condition) && !same_attr(x, y)) {
    return(diff_attr(x, y, id))
  }
}

get_diff.numeric <- function(x, y, eq_condition, id) {

  if (!typeof(y) %in% c("integer", "double")) {
    return(diff_type(x, y, id))
  }
  if (!same_class(x, y)) {
    return(diff_class(x, y, id))
  }
  if (!same_length(x, y)) {
    return(diff_length(x, y, id))
  }
  if (check_attr(eq_condition) && !same_attr(x, y)) {
    return(diff_attr(x, y, id))
  }
  
  return(NULL)
}


get_diff.character <- function(x, y, eq_condition, id) {
  if (!same_type(x, y)) {
    return(diff_type(x, y, id))
  }
  if (!same_class(x, y)) {
    return(diff_class(x, y, id))
  }
  if (!same_length(x, y)) {
    return(diff_length(x, y, id))
  }
  if (check_attr(eq_condition) && !same_attr(x, y)) {
    return(diff_attr(x, y, id))
  }
  return(NULL)
}


## Helpers for diff (from testthat)

# vector_equal <- function(x, y) {
#   (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
# }
# 
# vector_equal_tol <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
#   (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) < tolerance)
# }

same_length <- function(x, y) length(x) == length(y)
diff_length <- function(x, y, id) sprintf("%s has length %i, while it should have length %i.",
                                          id, length(y), length(x))

klass <- function(x) paste(class(x), collapse = "/")

same_type <- function(x, y) identical(typeof(x), typeof(y))
diff_type <- function(x, y, id) sprintf("%s has type `%s` while it should be `%s`.",
                                        id, typeof(y), typeof(x))

same_class <- function(x, y) {
  if (!is.object(x) && !is.object(y))
    return(TRUE)
  identical(class(x), class(y))
}
diff_class <- function(x, y, id) sprintf("%s is of class `%s` while it it should be `%s`.", id, klass(y), klass(x))

same_attr <- function(x, y) is.null(attr.all.equal(x, y))
diff_attr <- function(x, y, id) sprintf("are you sure the attributes (names, class, etc.) of %s are correct?", id)