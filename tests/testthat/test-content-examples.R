context("content_examples")
source("helpers.R")

test_that("exercise intermediate R", {
  lst <- list()
  lst$DC_CODE <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SOLUTION <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SCT <- 'test_object("day1")'
  output <- test_it(lst)
  passes(output)
})