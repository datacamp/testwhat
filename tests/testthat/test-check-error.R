context("check_error")

test_that("check_error", {
  lst <- list()
  lst$DC_SCT <- "ex () %>% check_error()"
  
  lst$DC_CODE <- "3 + 3"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "\"a\" + 3"
  output <- test_it(lst)
  fails(output)
})

test_that("check_error - backwards compatible", {
  lst <- list()
  lst$DC_SCT <- "test_error()"
  
  lst$DC_CODE <- "3 + 3"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "\"a\" + 3"
  output <- test_it(lst)
  fails(output)
})

test_that("check_error - line of error", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_error()"
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

test_that("check_error - incorrect_msg", {
  lst <- list()
  lst$DC_SCT <- "test_error(incorrect_msg = 'additionalmessage')"
  
  lst$DC_CODE <- "3 + 3"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "\"a\" + 3"
  output <- test_it(lst)
  fb_contains(output, "non-numeric argument to binary operator")
  fb_contains(output, "additionalmessage")
  fails(output)  
})