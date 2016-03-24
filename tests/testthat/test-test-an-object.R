context("test_an_object")
source("helpers.R")

test_that("test_an_object works", {
  lst <- list()
  lst$DC_SOLUTION <- "\n  var.equiv <- 3\n  var.other_equiv <- 4\n  var.not_here <- 2\n  var.not_equiv <- 5"
  lst$DC_CODE <- "\n  var.equiv <- 3\n  var.not_equiv <- 4"
  
  lst$DC_SCT <- "test_an_object('not_existing_in_solution')"
  output <- test_it(lst)
  error(output)
  
  lst$DC_SCT <- "test_an_object(\"var.not_equiv\")"
  output <- test_it(lst)
  fails(output, mess_patt = "There is some object missing in your code")
  
  lst$DC_SCT <- "test_an_object(\"var.not_here\", undefined_msg = \"This is the undefined message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "undefined message")
  
  lst$DC_SCT <- "test_an_object(\"var.equiv\", \"Testing 2\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_an_object(\"var.other_equiv\", \"Testing 3\")"
  output <- test_it(lst)
  passes(output)
  
  lst <- list()
  lst$DC_SOLUTION <- "\n  var.equiv <- 3\n  var.other_equiv <- 4\n  var.not_here <- 2\n  var.not_equiv <- 5"
  lst$DC_CODE <- "print('retteketet')"
  lst$DC_SCT <- "test_an_object('var.equiv')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_an_object works with eq_condition 1", {
  lst <- list()
  lst$DC_SOLUTION <- "\n  df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))\n  df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))"
  lst$DC_CODE <- "\n  df.equiv <- data.frame(a=c(10,11,12), b=c(4,5,6))\n  df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))"
  
  lst$DC_SCT <- "test_an_object(\"df.equiv\", \"Testing 4\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_an_object(\"df.equiv\", \"Testing 5\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_an_object(\"df.not_equiv\", \"Testing 6\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_an_object(\"df.not_equiv\", \"Testing 7\")"
  output <- test_it(lst)
  fails(output)
})


test_that("test_an_object works with eq_condition 2", {
  lst <- list()
  lst$DC_SOLUTION <- "var.iden <- 4\n  var.equal <- 3"
  lst$DC_CODE <- "var.iden <- 3 + 4.4e-8\n  var.equal <- 4"
  
  lst$DC_SCT <- "test_an_object(\"var.equal\", \"Testing 8\", eq_condition = \"identical\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_an_object(\"var.iden\", \"Testing 9\", eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_an_object(\"var.equal\", \"Testing 10\")"
  output <- test_it(lst)
  passes(output)
})

