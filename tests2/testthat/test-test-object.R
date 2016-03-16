context("test_object")
source("helpers.R")

test_that("test_object works for simple objects", {
  lst <- list()
  lst$DC_CODE <- "\n    var.equiv <- 3\n    var.not_equiv <- 4"
  lst$DC_SOLUTION <- "\n    var.equiv <- 3\n    var.not_equiv <- 3\n    var.not_here <- 2"
  
  lst$DC_SCT <- "test_object(\"var.not_equiv\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var.not_here\", undefined_msg = \"This is the undefined message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the undefined message")
  
  lst$DC_SCT <- "test_object(\"var.not_equiv\", incorrect_msg = \"This is the incorrect message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the incorrect message")
  
  lst$DC_SCT <- "test_object(\"var.equiv\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object works with eq_condition", {
  lst <- list()
  lst$DC_CODE <- "\n    df.equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))\n    df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))"
  lst$DC_SOLUTION <- "\n    df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))\n    df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))"
  
  lst$DC_SCT <- "test_object(\"df.equiv\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"df.not_equiv\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"df.not_equiv\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"df.equiv\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object works with eq_condition 2", {
  lst <- list()
  lst$DC_CODE <- "\n    var.iden <- 3\n    var.equal <- 3 + 4.4e-8"
  lst$DC_SOLUTION = "\n    var.iden <- 3\n    var.equal <- 3"
  
  lst$DC_SCT <- "test_object(\"var.equal\", eq_condition = \"identical\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var.iden\", eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_object(\"var.equal\", eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object works with eval", {
  lst <- list()
  lst$DC_CODE <- "var <- 3\nvar.other <- 4"
  lst$DC_SOLUTION <- "var <- 5\nvar.other <- 3\nvar.not_here<-7\n"
  
  lst$DC_SCT <- "test_object(\"var\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var.not_here\", eval = FALSE)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var\", eval = FALSE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object wroks with 'like' eq_condition", {
  lst <- list()
  lst$DC_CODE <- "var <- \"Filip stinkt\"\nvar2 <- \"Filip is leuk\"\n"
  lst$DC_SOLUTION <- "var <- \"Filip drinkt\"\nvar2 <- \"Filip is stom\"\n"
  
  lst$DC_SCT <- "test_object(\"var2\", eq_condition = \"like\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var2\", eq_condition = \"like\", incorrect_msg = \"Too different\")"
  output <- test_it(lst)
  fails(output, mess_patt = "Too different")
  
  lst$DC_SCT <- "test_object(\"var\", eq_condition = \"like\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object passes along correct line numbers", {
  lst <- list()
  lst$DC_CODE <- "a <- 5\n\ny <- 4\n\nz <- 6\n\nz <- 7"
  lst$DC_SOLUTION <- "x <- 4\ny <- 5\nz <- 6"
  
  # If no assignments in student code, show nothing
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
  
  # If single assignment in student code, show line
  lst$DC_SCT <- "test_object('y')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 3, 3)
  
  # If two assignments in student code, show nothing
  lst$DC_SCT <- "test_object('z')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
})

test_that("test_object with line numbers - 2", {
  lst <- list()
  lst$DC_CODE <- "5 -> x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "x <<- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "5 ->> x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  # use of equality sign doesn't show yet!
  lst <- list()
  lst$DC_CODE <- "x = 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
})
