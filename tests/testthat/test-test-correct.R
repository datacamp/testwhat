context("test_correct")

test_that("diagnose and check - ", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- mean(1:11)"
  lst$DC_SCT <- "ex() %>% test_obj('x') %>% check() %>% test_equal() %>% diagnose() %>% test_fun('mean') %>% test_arg('x') %>% test_equal() %>% done()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define the variable <code>x</code> without errors")

  lst$DC_CODE <- "x <- 8"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code> .*? Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- mean(1:123)"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code>")
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  fails(output, mess_patt = "It has length 123, while it should have length 11")

  lst$DC_CODE <- "x <- mean(1:11) + 2"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code>")

  lst$DC_CODE <- "x <- mean(1:11)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "x <- 6"
  output <- test_it(lst)
  passes(output)
})



# test_that("test_correct works", {
#   lst <- list()
#   lst$DC_CODE <- "var.equiv <- 3\n  var.not_equiv <- 4\n  mean(var.equiv)"
#   lst$DC_SOLUTION <- "var.equiv <- 3\n  var.not_equiv <- 3\n  var.not_here <- 2\n  mean(var.equiv)"
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.equiv\")", "}, {", 
#                       "    test_function(\"mean\")", "})", sep= "\n")
#   output <- test_it(lst)
#   passes(output)
#   
#   lst <- list()
#   lst$DC_CODE <- "var.equiv <- 3\n  var.not_equiv <- 4\n  mean(var.not_equiv)"
#   lst$DC_SOLUTION <- "var.equiv <- 3\n  var.not_equiv <- 3\n  var.not_here <- 2\n  mean(var.not_equiv)"
#   
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.equiv\")",
#                       "    test_function(\"mean\")", 
#                       "}, {", 
#                       "    test_function(\"mean\", \"x\")", "})", sep= "\n")
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.equiv\")", 
#                       "    test_function(\"mean\", \"x\", incorrect_msg = \"This is the one\")",
#                       "}, {",
#                       "    test_function(\"mean\")", "})", sep = "\n")
#   output <- test_it(lst)
#   fails(output, mess_patt = "This is the one")
#   
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.not_equiv\", incorrect_msg = \"Wrong object!\")", 
#                       "    test_function(\"mean\", \"x\", incorrect_msg = \"This is the one\")", 
#                       "}, {", 
#                       "    test_object(\"var.equiv\")", 
#                       "})", sep = "\n")
#   output <- test_it(lst)
#   fails(output, mess_patt = "Wrong object!")
#   
#   
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.not_equiv\", incorrect_msg = \"Don't send this\")", 
#                       "}, {", 
#                       "    test_function(\"mean\", \"x\", incorrect_msg = \"Diagnose of the problem\")", 
#                       "})", sep = "\n")
#   output <- test_it(lst)
#   fails(output, mess_patt = "Diagnose of the problem")
#   
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.not_equiv\", incorrect_msg = \"Don't send this\")", 
#                       "    test_object(\"var.equiv\")", "    test_function(\"mean\", \"x\", incorrect_msg = \"Wrong message\")", 
#                       "}, {", 
#                       "    test_function(\"mean\", not_called_msg = \"Diagnose of the problem\")", 
#                       "    test_function(\"mean\", \"x\", incorrect_msg = \"This is the one!\")", 
#                       "})", sep = "\n")
#   output <- test_it(lst)
#   fails(output, mess_patt = "This is the one!")
#   
#   lst$DC_SCT <- paste("test_correct({", 
#                       "    test_object(\"var.not_equiv\", incorrect_msg = \"It will send this oneeee\")",
#                       "    test_object(\"var.equiv\")", 
#                       "    test_function(\"mean\", \"x\", incorrect_msg = \"Wrong message\")", 
#                       "}, {", 
#                       "    test_function(\"mean\", not_called_msg = \"Diagnose of the problem\")", "})", sep = "\n")
#   output <- test_it(lst)
#   fails(output, mess_patt = "It will send this oneeee")
# })
# 
# test_that("test_correct throws errors if incorrect diagnose_code", {
#   lst <- list()
#   lst$DC_CODE <- "a <- 2; b <- 2; c <- a + b"
#   lst$DC_SOLUTION <- "b <- 2; c <- 2 + b"
#   
#   lst$DC_SCT <- "test_correct(test_object('c'), test_object('b'))"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- "test_correct(test_object('c'), test_object('a'))"
#   output <- test_it(lst)
#   error(output)
#   
#   lst <- list()
#   lst$DC_CODE <- "x <- nchar('test')"
#   lst$DC_SOLUTION <- "x <- nchar('test')"
#   lst$DC_SCT <- "test_correct(test_object('x'), test_function('nchar', 'x'))"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst <- list()
#   lst$DC_CODE <- "x <- nchar('tester')"
#   lst$DC_SOLUTION <- "x <- nchar('test')"
#   lst$DC_SCT <- "test_correct(test_object('x'), test_function('nchar', 'x'))"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst <- list()
#   lst$DC_CODE <- "x <- nchar('tester')"
#   lst$DC_SOLUTION <- "x <- nchar('test')"
#   lst$DC_SCT <- "test_correct(test_object('x'), test_function('nchar', 'object'))"
#   output <- test_it(lst)
#   error(output)
#   lst$DC_CODE <- "x <- nchar('test')"
#   output <- test_it(lst)
#   error(output)
# })
# 
# test_that("test_correct works in nested form", {
#   lst <- list()
#   lst$DC_SOLUTION <- "a <- 1:5; b <- mean(a)"
#   lst$DC_SCT <- "test_correct(test_object('b', incorrect_msg = 'b_incorrect'), test_correct(test_function('mean', args = 'x', incorrect_msg = 'mean_incorrect'), test_object('a', incorrect_msg = 'a_incorrect')))"
#   
#   lst$DC_CODE <- "a <- 1:5; b <- mean(a)"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_CODE <- "b <- 3"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_CODE <- "a <- 1:4; b <- mean(a)"
#   output <- test_it(lst)
#   fails(output, mess_patt = 'a_incorrect')
#   
#   lst$DC_CODE <- "a <- 1:5; b <- mean(a + 1)"
#   output <- test_it(lst)
#   fails(output, mess_patt = 'mean_incorrect')
#   
#   lst$DC_CODE <- "a <- 1:5; b <- mean(a) + 1"
#   output <- test_it(lst)
#   fails(output, mess_patt = 'b_incorrect')
# })