build_diff <- function(...) {
  res <- get_diff(...)
  if (is.null(res)) {
    return(NULL)
  } else {
    # capitalize + space in front
    return(paste0(" ", toupper(substring(res, 1, 1)), substring(res, 2)))
  }
}

get_diff <- function(sol, stud, ...) {
  UseMethod("get_diff", sol)
}

get_diff.default <- function(sol, ...) {
  return(NULL)
}

## Class-specific diffs

get_diff.logical <- function(sol, stud, eq_condition, id) {
  if (!typeof(stud) %in% c("logical", "integer", "double")) {
    return(diff_type(sol, stud, id))
  }
  if (!same_class(sol, stud)) {
    return(diff_class(sol, stud, id))
  }
  if (!same_length(sol, stud)) {
    return(diff_length(sol, stud, id))
  }
  if (check_attr(eq_condition) && !same_attr(sol, stud)) {
    return(diff_attr(sol, stud, id))
  }
  return(NULL)
}

get_diff.numeric <- function(sol, stud, eq_condition, id) {

  if (!typeof(stud) %in% c("integer", "double")) {
    return(diff_type(sol, stud, id))
  }
  if (!same_class(sol, stud)) {
    return(diff_class(sol, stud, id))
  }
  if (!same_length(sol, stud)) {
    return(diff_length(sol, stud, id))
  }
  if (check_attr(eq_condition) && !same_attr(sol, stud)) {
    return(diff_attr(sol, stud, id))
  }
  return(NULL)
}


get_diff.character <- function(sol, stud, eq_condition, id) {
  if (!same_type(sol, stud)) {
    return(diff_type(sol, stud, id))
  }
  if (!same_class(sol, stud)) {
    return(diff_class(sol, stud, id))
  }
  if (!same_length(sol, stud)) {
    return(diff_length(sol, stud, id))
  }
  if (check_attr(eq_condition) && !same_attr(sol, stud)) {
    return(diff_attr(sol, stud, id))
  }
  return(NULL)
}

get_diff.data.frame <- function(sol, stud, eq_condition, id) {
  if (!same_type(sol, stud)) {
    return(diff_type(sol, stud, id))
  }
  if (!same_class(sol, stud)) {
    return(diff_class(sol, stud, id))
  }
  if (!same_dim(sol, stud)) {
    return(diff_dim(sol, stud, id))
  }
  if (check_attr(eq_condition) && !same_attr(sol, stud)) {
    return(diff_attr(sol, stud, id))
  }
}


same_length <- function(sol, stud) length(sol) == length(stud)
diff_length <- function(sol, stud, id) sprintf("%s has length %i, while it should have length %i.",
                                          id, length(stud), length(sol))

same_dim <- function(sol, stud) isTRUE(all.equal(dim(sol), dim(stud)))
diff_dim <- function(sol, stud, id) {
  stud_rows <- dim(stud)[1]
  stud_cols <- dim(stud)[2]
  sol_rows <- dim(sol)[1]
  sol_cols <- dim(sol)[2]
  plural <- function(x) {
    ifelse(x > 1, "s", "")
  }
  sprintf("%s has %i row%s and %i column%s, while it should have %i row%s and %i column%s.",
          id, stud_rows, plural(stud_rows), stud_cols, plural(stud_cols),
          sol_rows, plural(sol_rows), sol_cols, plural(sol_cols))
}

klass <- function(sol) paste(class(sol), collapse = "/")

same_type <- function(sol, stud) identical(typeof(sol), typeof(stud))
diff_type <- function(sol, stud, id) sprintf("%s has type `%s`, while it should have type `%s`.",
                                        id, typeof(stud), typeof(sol))

same_class <- function(sol, stud) {
  if (!is.object(sol) && !is.object(stud))
    return(TRUE)
  identical(class(sol), class(stud))
}
diff_class <- function(sol, stud, id) sprintf("%s is of class `%s`, while it should be of class `%s`.", id, klass(stud), klass(sol))

same_attr <- function(sol, stud) is.null(attr.all.equal(sol, stud))
diff_attr <- function(sol, stud, id) sprintf("are you sure the attributes (names, class, etc.) of %s are correct?", id)
