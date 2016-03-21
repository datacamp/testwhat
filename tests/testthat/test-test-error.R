context("test_error")
source("helpers.R")

test_that("basic test_error", {
  lst <- list()
  lst$DC_CODE <- "3 + 3"
  
  lst$DC_SCT <- "test_error()"
  output <- test_it(lst)
  passes(output)
  
  lst <- list()
  lst$DC_CODE <- "\"a\" + 3"
  
  lst$DC_SCT <- "test_error()"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_error(\"this is the incorrect msg\")"
  output <- test_it(lst)
  fails(output, mess_patt = "this is the incorrect msg")
})

test_that("test_error passed the correct line of error", {
  lst <- list()
  lst$DC_SCT <- "test_error()"
  lst$DC_ECHO <- TRUE # This is important here!
  
  lst$DC_CODE <- "a <- b"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst$DC_CODE <- "a <- b\na <- b"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst$DC_CODE <- "b <- 4\na <- b\nrm(b)\na <- b"
  output <- test_it(lst)
  fails(output)
  line_info(output, 4, 4)

  lst$DC_CODE <- "sum(\"a\", \n\n  \"b\")"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 3)
})