context("test_correct")
source("helpers.R")

test_that("test_correct works", {
  lst <- list()
  lst$DC_CODE <- "var.equiv <- 3\n  var.not_equiv <- 4\n  mean(var.equiv)"
  lst$DC_SOLUTION <- "var.equiv <- 3\n  var.not_equiv <- 3\n  var.not_here <- 2\n  mean(var.equiv)"
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.equiv\")", "}, {", 
                      "    test_function(\"mean\")", "})", sep= "\n")
  output <- test_it(lst)
  passes(output)
  
  lst <- list()
  lst$DC_CODE <- "var.equiv <- 3\n  var.not_equiv <- 4\n  mean(var.not_equiv)"
  lst$DC_SOLUTION <- "var.equiv <- 3\n  var.not_equiv <- 3\n  var.not_here <- 2\n  mean(var.not_equiv)"
  
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.equiv\")",
                      "    test_function(\"mean\")", 
                      "}, {", 
                      "    test_function(\"mean\", \"x\")", "})", sep= "\n")
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.equiv\")", 
                      "    test_function(\"mean\", \"x\", incorrect_msg = \"This is the one\")",
                      "}, {",
                      "    test_function(\"mean\")", "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "This is the one")
  
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.not_equiv\", incorrect_msg = \"Wrong object!\")", 
                      "    test_function(\"mean\", \"x\", incorrect_msg = \"This is the one\")", 
                      "}, {", 
                      "    test_object(\"var.equiv\")", 
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "Wrong object!")
  
  
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.not_equiv\", incorrect_msg = \"Don't send this\")", 
                      "}, {", 
                      "    test_function(\"mean\", \"x\", incorrect_msg = \"Diagnose of the problem\")", 
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "Diagnose of the problem")
  
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.not_equiv\", incorrect_msg = \"Don't send this\")", 
                      "    test_object(\"var.equiv\")", "    test_function(\"mean\", \"x\", incorrect_msg = \"Wrong message\")", 
                      "}, {", 
                      "    test_function(\"mean\", not_called_msg = \"Diagnose of the problem\")", 
                      "    test_function(\"mean\", \"x\", incorrect_msg = \"This is the one!\")", 
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "This is the one!")
  
  lst$DC_SCT <- paste("test_correct({", 
                      "    test_object(\"var.not_equiv\", incorrect_msg = \"It will send this oneeee\")",
                      "    test_object(\"var.equiv\")", 
                      "    test_function(\"mean\", \"x\", incorrect_msg = \"Wrong message\")", 
                      "}, {", 
                      "    test_function(\"mean\", not_called_msg = \"Diagnose of the problem\")", "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "It will send this oneeee")
})

test_that("test_correct works in testing mode", {
  lst <- list()
  lst$DC_CODE <- "a <- 2; b <- 3; c <- a + b"
  lst$DC_SOLUTION <- "a <- 3; b <- 2; c <- a + b"
  lst$DC_SCT <- "test_correct(test_object('c'), { test_object('a'); test_object('c') })"
  
  lst$DC_TEST_MODE <- FALSE
  output <- test_it(lst)
  passes(output)
  
  lst$DC_TEST_MODE <- TRUE
  output <- test_it(lst)
  fails(output, mess_patt = "<code>a</code>")
})

test_that("test_correct works in nested form", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 1:5; b <- mean(a)"
  lst$DC_SCT <- "test_correct(test_object('b', incorrect_msg = 'b_incorrect'), test_correct(test_function('mean', args = 'x', incorrect_msg = 'mean_incorrect'), test_object('a', incorrect_msg = 'a_incorrect')))"
  
  lst$DC_CODE <- "a <- 1:5; b <- mean(a)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "b <- 3"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "a <- 1:4; b <- mean(a)"
  output <- test_it(lst)
  fails(output, mess_patt = 'a_incorrect')
  
  lst$DC_CODE <- "a <- 1:5; b <- mean(a + 1)"
  output <- test_it(lst)
  fails(output, mess_patt = 'mean_incorrect')
  
  lst$DC_CODE <- "a <- 1:5; b <- mean(a) + 1"
  output <- test_it(lst)
  fails(output, mess_patt = 'b_incorrect')
})