context("test_expression_error")

test_that("test_expression_result works", {
  lst <- list()
  lst$DC_SOLUTION <- paste("func <- function(x) { stop('error') }",
                       "func2 <- function(x) { stop('error') }",
                       "func3 <- function(x) { stop('error') }", sep = "\n")
  lst$DC_CODE <- paste("func <- function(x) { stop('error') }",
                           "func2 <- function(x) { message('noerror') }",
                           "func3 <- function(x) { stop('wrongerror') }", sep = "\n")
  
  lst$DC_SCT <- "test_expression_error(\"func(3)\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_expression_error(\"func2(3)\")"
  output <- test_it(lst)
  fails(output, mess_patt = "It seems that running")
  
  lst$DC_SCT <- "test_expression_error(\"func2(3)\", no_error_msg = \"theresnoerror\")"
  output <- test_it(lst)
  fails(output, mess_patt = "theresnoerror")
  
  lst$DC_SCT <- "test_expression_error(\"func3(3)\")"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure that running.*generates the following error.*")
  
  lst$DC_SCT <- "test_expression_error(\"func3(3)\", incorrect_msg = \"notgood\")"
  output <- test_it(lst)
  fails(output, mess_patt = "notgood")
})
