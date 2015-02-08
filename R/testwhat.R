#' Easily write submission correctness tests
#'
#' This package makes it easy for teachers to test students' code submissions
#' for R exercises, e.g., for interactive R courses on \url{www.DataCamp.com}.
#'
#' @docType package
#' @name testwhat
#'
#' @note \pkg{testwhat} is forked from package \pkg{testthat} such that
#' teachers can write their tests using a familiar framework.
#'
#' @references \url{www.DataCamp.com}
#'
#' @examples
#' library("testwhat")
#' a <- 9
#' expect_that(a, is_less_than(10))
#' expect_less_than(a, 10)
NULL
