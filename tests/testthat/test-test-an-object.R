context("test_ggplot")

test_that("test_an_object", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 2"
  lst$DC_SCT <- "test_an_object('a')"
  
  lst$DC_CODE <- "a <- 2"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "b <- 2"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "a <- 3"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "b <- 3"
  output <- test_it(lst)
  fails(output)
})
  