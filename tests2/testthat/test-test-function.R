context("test_function")
source("helpers.R")

test_that("basic test_function", {
  lst <- list()
  lst$DC_CODE <- "summary(c(1,2,3,4))"
  lst$DC_SOLUTION <- "summary(c(1,2,3,4),c(1,2,3,4))\n  dim(c(1,2,3))"
  
  lst$DC_SCT <- "test_function(\"summary\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function(\"dim\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_function(\"dim\", not_called_msg = \"This is the not called message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the not called message")
})

test_that("test_function, checking arguments", {
  lst <- list()
  lst$DC_CODE <- "summary(c(1,2,3,4))\n  dim(c(1,2,3))\n  rep(1, 4)\n  dnorm(1, 10, 5)\n  mean(c(1,2,3), na.rm = FALSE)"
  lst$DC_SOLUTION <- "summary(c(1,2,3,4))\n  dim(c(1,2,3,4))\n  rep(1, 20)\n  dnorm(1,10)\n  mean(c(1,2,3), na.rm = TRUE)"
  
  lst$DC_SCT <- "test_function(\"summary\", \"object\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function(\"rep\", \"x\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function(\"dnorm\", c(\"x\", \"mean\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function(\"dim\", \"x\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_function(\"dnorm\", c(\"x\", \"mean\"), allow_extra = FALSE)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_function(\"mean\", c(\"x\", \"na.rm\"))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_function(\"dim\", \"x\", incorrect_msg = \"This is the incorrect message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the incorrect message")
})

test_that("test_function, eq_condition", {
  lst <- list()
  lst$DC_CODE <- "df.equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))\n  var(df.equiv)\n  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))\n  lm(df.not_equiv)"
  lst$DC_SOLUTION <- "df.equiv <- data.frame(c = c(1, 2, 3), d = c(4, 5, 6))\n  var(df.equiv)\n  df.not_equiv <- data.frame(c = c(7, 8, 9), d = c(4, 5, 6))\n  lm(df.not_equiv)"
  
  lst$DC_SCT <- "test_function(\"var\", \"x\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function(\"lm\", \"formula\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_function(\"var\", \"x\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_function(\"lm\", \"formula\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
})

test_that("test_function works with function_usage bookkeeping", {
  lst <- list()
  lst$DC_PEC <- 'emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org", 
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")'
  lst$DC_CODE <- 'sub("edu", "edu", emails)\nsub("edu", "edu", emails)'
  lst$DC_SOLUTION <- lst$DC_CODE
  lst$DC_SCT <- paste('test_function("sub", "pattern", index = 1)\n',
                      'test_function("sub", "replacement", index = 1)\n',
                      'test_function("sub", "x", index = 1)\n',
                      'test_function("sub", "pattern", index = 2)\n',
                      'test_function("sub", "replacement", index = 2)\n',
                      'test_function("sub", "x", index = 2)')
  output <- test_it(lst)
  passes(output)
})

test_that("test_function passes along correct line numbers", {
  lst <- list()
  lst$DC_CODE <- "mean(1:20)"
  lst$DC_SOLUTION <- "mean(1:15)"
  lst$DC_SCT <- "test_function('mean', args = 'x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  # Should select the function call with 'highest arg matching score'
  lst <- list()
  lst$DC_CODE <- "mean(1:20, trim = 0.2, na.rm = FALSE)\n\nmean(1:20, \ntrim = 0.1, \nna.rm = FALSE)"
  lst$DC_SOLUTION <- "mean(1:20, trim = 0.1, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim', 'na.rm'))"
  output <- test_it(lst)
  fails(output)
  line_info(output, 3, 5)
})