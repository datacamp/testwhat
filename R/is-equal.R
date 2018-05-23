invalid_eq_condition <- "eq_condition should be either 'equivalent', 'equal' or 'identical'."

#' Check equality of two objects
#'
#' Utility function to use inside \code{\link{check_that}}.
#'
#' @param x object to test
#' @param y object to compare
#' @param eq_condition how to compare the objects: \code{"equivalent"} (the default,
#'   does not check attributes), \code{"equal"} (checks attributes, but allows for
#'   errors in machine precision), or \code{"identical"} (exactly identical).
#' @name is_equal

#' @rdname is_equal
#' @export
is_equal <- function(x, y, eq_condition = "equivalent") {
  UseMethod("is_equal", x)
}

#' @rdname is_equal
#' @export
is_equal.default <- function(x, y, eq_condition = "equivalent") {
  eq_fun <- switch(eq_condition,
                   equivalent = function(x, y) isTRUE(try(all.equal(x, y, check.attributes = FALSE), silent = TRUE)),
                   equal = function(x, y) isTRUE(try(all.equal(x, y), silent = TRUE)),
                   identical = identical,
                   stop(invalid_eq_condition))
  eq_fun(x, y)
}

#' @rdname is_equal
#' @export
is_equal.formula <- function(x, y, eq_condition = "equivalent") {
  tryCatch({
    xlst <- convert_formula(x)
    ylst <- convert_formula(y)
    isTRUE(all.equal(xlst$target, ylst$target)) &&
      isTRUE(all.equal(xlst$explan, ylst$explan))
  }, error = function(e) {
    # fallback to default equality
    is_equal.default(x, y, eq_condition)
  })
}

convert_formula <- function(form) {
  n <- length(form)
  deparsed <- lapply(form, deparse)[2:n]
  target <- deparsed[[1]]
  explan <- deparsed[[2]]
  trm <- function(x) gsub("^\\s+|\\s+$", "", x)
  explan <- sort(trm(strsplit(explan, "\\+")[[1]]))
  return(list(target = target, explan = explan))
}
