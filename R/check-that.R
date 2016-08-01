#' Expectation wrapper
#' 
#' This function wraps around an is_... function. When the expectation fails to
#' be met, the feedback message is sent to the reporter. You can use 
#' \code{\link{is_true}}, \code{\link{is_false}}, \code{\link{is_gte}} or
#' \code{\link{is_equal}}
#' 
#' @param code The expectation that should be wrapped
#' @param feedback A character string with feedback when the expection is not
#'   met OR a list object, containing multiple pieces of information. This list
#'   should at least contain an element named \code{message}
#' @param env environment in which the test should be evaluated; defaults to \code{parent.frame()}
#'   
#' @examples
#' \dontrun{
#' check_that(is_true(3 == 3))
#' check_that(is_false(3 == 4))
#' check_that(is_gte(4, 3))
#' check_that(is_equal(4, 4))
#' }
#' 
#' @export
check_that <- function(code, feedback, env = parent.frame()) {
  
  # feedback can be a character string
  if (is.character(feedback)) {
    feedback <- list(message = feedback)
  } 
  
  if (!is.list(feedback) || 
      !("message" %in% names(feedback)) || 
      is.null(feedback$message) || 
      !is.character(feedback$message) || 
      nchar(feedback$message) == 0) {
    stop("The feedback you specified in check_that() isn't in the correct format")
  }

  res <- try(eval(code, envir = env), silent = TRUE)
  if (!isTRUE(res)) {
    get_rep()$set_feedback(c(feedback, list(tags = tw$get("tags"))))
    stop(sct_failed_msg)
  }
}

# for backwords compatibility
test_what <- function(code, feedback) {
  lut <- list(expect_true = call("is_true"),
              expect_false = call("is_false"),
              expect_equal = call("is_equal"))
  call <- substitute(code)
  call[1] <- lut[[as.character(call[[1]])]]
  check_that(call, feedback, env = parent.frame())
}

#' Check if object is true
#' 
#' Utility function to use inside \code{\link{check_that}}.
#' 
#' @param x object to test
#' @export
is_true <- function(x) {
  identical(as.vector(x), TRUE)
}

#' Check if object is false
#' 
#' Utility function to use inside \code{\link{check_that}}.
#' 
#' @param x object to test
#' @export
is_false <- function(x) {
  identical(as.vector(x), FALSE)
}

#' Check >= relation
#' 
#' Utility function to use inside \code{\link{check_that}}.
#' 
#' @param x object to test
#' @param y single numeric value to compare
#' @export
is_gte <- function(x, y) {
  stopifnot(is.numeric(x), length(x) == 1)
  stopifnot(is.numeric(y), length(y) == 1)
  x >= y
}

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
#' @export
is_equal <- function(x, y, eq_condition = "equivalent") {
  eq_fun <- switch(eq_condition,
                   equivalent = function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE)),
                   equal = function(x, y) isTRUE(all.equal(x, y)),
                   identical = identical,
                   stop(invalid_eq_condition))
  eq_fun(x, y)
}

failure <- function() {
  FALSE
}
