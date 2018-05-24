context("parsing")

check_parse <- function(code, patt, ls = NULL, cs = NULL, le = NULL, ce = NULL) {
  res <- do_parse(code)
  expect_true(grepl(patt, res$message, fixed = TRUE))
  if (!is.null(ls)) expect_equal(res$line_start, ls)
  if (!is.null(cs)) expect_equal(res$column_start, cs)
  if (!is.null(le)) expect_equal(res$line_end, le)
  if (!is.null(ce)) expect_equal(res$column_end, ce)
}

test_that("unit test: unimplemented parse errors good", {
  check_parse("x <- (1, 2, 3)", patt = parse_fallback_msg)
  check_parse("x a b", patt = parse_fallback_msg)
  check_parse("for () {}", patt = parse_fallback_msg)
})

test_that("unit test: brackets or braces incorrect", {
  c1a <- "x <- c(1, 2, 3"
  c1b <- "# comment\nx <- c(1, 2, 3"
  c1c <- "y <- 'just a string'\nx <- c(1, 2, 3"
  c1d <- "x <- c(1, 2, 3\ny <- 'just a string'"
  patt <- "Don't forget to close this parenthesis"
  check_parse(c1a, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  check_parse(c1b, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c1c, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c1d, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  
  c2a <- "x <- c(1, 2, 3]"
  c2b <- "# comment\nx <- c(1, 2, 3]"
  c2c <- "y <- 'just a string'\nx <- c(1, 2, 3]"
  c2d <- "x <- c(1, 2, 3]\ny <- 'just a string'"
  patt <- "Make sure to correctly close this parenthesis"
  check_parse(c2a, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  check_parse(c2b, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c2c, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c2d, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  
  c3a <- "x <- c(1, 2, 3}"
  c3b <- "# comment\nx <- c(1, 2, 3}"
  c3c <- "y <- 'just a string'\nx <- c(1, 2, 3}"
  c3d <- "x <- c(1, 2, 3}\ny <- 'just a string'"
  patt <- "Make sure to correctly close this parenthesis"
  check_parse(c3a, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  check_parse(c3b, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c3c, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c3d, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  
  c4a <- "x <- c(1, 2, 3))"
  c4b <- "# comment\nx <- c(1, 2, 3))"
  c4c <- "y <- 'just a string'\nx <- c(1, 2, 3))"
  c4d <- "x <- c(1, 2, 3))\ny <- 'just a string'"
  patt <- "This parenthesis wasn't expected"
  check_parse(c4a, patt = patt, ls = 1, cs = 16, le = 1, ce = 16)
  check_parse(c4b, patt = patt, ls = 2, cs = 16, le = 2, ce = 16)
  check_parse(c4c, patt = patt, ls = 2, cs = 16, le = 2, ce = 16)
  check_parse(c4d, patt = patt, ls = 1, cs = 16, le = 1, ce = 16)
  
  c5a <- "x <- c(1, 2, 3)]"
  c5b <- "# comment\nx <- c(1, 2, 3)]"
  c5c <- "y <- 'just a string'\nx <- c(1, 2, 3)]"
  c5d <- "x <- c(1, 2, 3)]\ny <- 'just a string'"
  patt <- "This bracket wasn't expected"
  check_parse(c5a, patt = patt, ls = 1, cs = 16, le = 1, ce = 16)
  check_parse(c5b, patt = patt, ls = 2, cs = 16, le = 2, ce = 16)
  check_parse(c5c, patt = patt, ls = 2, cs = 16, le = 2, ce = 16)
  check_parse(c5d, patt = patt, ls = 1, cs = 16, le = 1, ce = 16)
  
  c6a <- "x <- c((1, 2, 3)"
  c6b <- "# comment\nx <- c((1, 2, 3)"
  c6c <- "y <- 'just a string'\nx <- c((1, 2, 3)"
  c6d <- "x <- c((1, 2, 3)\ny <- 'just a string'"
  patt <- "Don't forget to close this parenthesis"
  check_parse(c6a, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  check_parse(c6b, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c6c, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c6d, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  
  c7a <- "x <- y[1, 2, 3)"
  c7b <- "# comment\nx <- y[1, 2, 3)"
  c7c <- "y <- 'just a string'\nx <- y[1, 2, 3)"
  c7d <- "x <- y[1, 2, 3)\ny <- 'just a string'"
  patt <- "Make sure to correctly close this bracket"
  check_parse(c7a, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  check_parse(c7b, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c7c, patt = patt, ls = 2, cs = 7, le = 2, ce = 7)
  check_parse(c7d, patt = patt, ls = 1, cs = 7, le = 1, ce = 7)
  
  c8a <- "x <- y[1, 2, 3]]"
  c8b <- "# comment\nx <- y[1, 2, 3]]"
  c8c <- "y <- 'just a string'\nx <- y[1, 2, 3]]"
  c8d <- "x <- y[1, 2, 3]]\ny <- 'just a string'"
  patt <- "This bracket wasn't expected"
  check_parse(c8a, patt = patt, ls = 1, cs = 16, le = 1, ce = 16)
  check_parse(c8b, patt = patt, ls = 2, cs = 16, le = 2, ce = 16)
  check_parse(c8c, patt = patt, ls = 2, cs = 16, le = 2, ce = 16)
  check_parse(c8d, patt = patt, ls = 1, cs = 16, le = 1, ce = 16)
  
  c9a <- "x <- y[[1, 2, 3)"
  c9b <- "# comment\nx <- y[[1, 2, 3)"
  c9c <- "y <- 'just a string'\nx <- y[[1, 2, 3)"
  c9d <- "x <- y[[1, 2, 3)\ny <- 'just a string'"
  patt <- "Make sure to correctly close this double bracket"
  check_parse(c9a, patt = patt, ls = 1, cs = 7, le = 1, ce = 8)
  check_parse(c9b, patt = patt, ls = 2, cs = 7, le = 2, ce = 8)
  check_parse(c9c, patt = patt, ls = 2, cs = 7, le = 2, ce = 8)
  check_parse(c9d, patt = patt, ls = 1, cs = 7, le = 1, ce = 8)
})

test_that("unit test: unexpected string end", {
  patt <- "Make sure to close the string again!"
  
  c1a <- "'incomplete_string"
  c1b <- "# comment\n'incomplete_string"
  c1c <- "y <- 'just a string'\n'incomplete_string"
  c1d <- "'incomplete_string\ny <- 'just a string'"
  check_parse(c1a, patt = patt, ls = 1, cs = 1, le = 1, ce = 1)
  check_parse(c1b, patt = patt, ls = 2, cs = 1, le = 2, ce = 1)
  check_parse(c1c, patt = patt, ls = 2, cs = 1, le = 2, ce = 1)
  check_parse(c1d, patt = patt, ls = 2, cs = 20, le = 2, ce = 20)
  
  c2a <- "\"incomplete_string"
  c2b <- "# comment\n\"incomplete_string"
  c2c <- "y <- 'just a string'\n\"incomplete_string"
  c2d <- "\"incomplete_string\ny <- 'just a string'"
  check_parse(c2a, patt = patt, ls = 1, cs = 1, le = 1, ce = 1)
  check_parse(c2b, patt = patt, ls = 2, cs = 1, le = 2, ce = 1)
  check_parse(c2c, patt = patt, ls = 2, cs = 1, le = 2, ce = 1)
  check_parse(c2d, patt = patt, ls = 1, cs = 1, le = 1, ce = 1)
  
  c3a <- "\"incomplete_string(()"
  c3b <- "# comment\n\"incomplete_string(()"
  c3c <- "y <- 'just a string'\n\"incomplete_string(()"
  c3d <- "\"incomplete_string(()\ny <- 'just a string'"
  check_parse(c3a, patt = patt, ls = 1, cs = 1, le = 1, ce = 1)
  check_parse(c3b, patt = patt, ls = 2, cs = 1, le = 2, ce = 1)
  check_parse(c3c, patt = patt, ls = 2, cs = 1, le = 2, ce = 1)
  check_parse(c3d, patt = patt, ls = 1, cs = 1, le = 1, ce = 1)
})

test_that("unit test: ___ in the string", {
  # only track ___ 
  check_parse("mean(__, na.rm = TRUE)", patt = parse_fallback_msg)
  
  patt <- "Replace it with valid R code!"
  
  c1a <- "mean(___, na.rm = TRUE)"
  c1b <- "# comment\nmean(___, na.rm = TRUE)"
  c1c <- "x <- 'just a string'\nmean(___, na.rm = TRUE)"
  check_parse(c1a, patt = patt, ls = 1, cs = 6, le = 1, ce = 8)
  check_parse(c1b, patt = patt, ls = 2, cs = 6, le = 2, ce = 8)
  check_parse(c1c, patt = patt, ls = 2, cs = 6, le = 2, ce = 8)
  
  c2a <- "mean(___, na.rm = TRUE)\n___ + ____"
  c2b <- "# comment\nmean(___, na.rm = TRUE)\n___ + ____"
  c2c <- "x <- 'just a string'\nmean(___, na.rm = TRUE)\n___ + ____"
  check_parse(c2a, patt = patt, ls = 1, cs = 6, le = 1, ce = 8)
  check_parse(c2b, patt = patt, ls = 2, cs = 6, le = 2, ce = 8)
  check_parse(c2c, patt = patt, ls = 2, cs = 6, le = 2, ce = 8)
  
  c3a <- "___"
  c3b <- "# comment\n___"
  c3c <- "x <- 'just a string'\n___"
  check_parse(c3a, patt = patt, ls = 1, cs = 1, le = 1, ce = 3)
  check_parse(c3b, patt = patt, ls = 2, cs = 1, le = 2, ce = 3)
  check_parse(c3c, patt = patt, ls = 2, cs = 1, le = 2, ce = 3)
  
  c4a <- "a___\n___"
  c4b <- "a3___\n___"
  c4c <- "# comment\nb___\n___"
  check_parse(c4a, patt = patt, ls = 2, cs = 1, le = 2, ce = 3)
  check_parse(c4b, patt = patt, ls = 2, cs = 1, le = 2, ce = 3)
  check_parse(c4c, patt = patt, ls = 3, cs = 1, le = 3, ce = 3)
})

