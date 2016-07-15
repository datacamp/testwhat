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


test_that("test_function, eval stuff", {
  lst <- list()
  lst$DC_CODE <- "df.equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))\n  var(df.equiv)\n  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))\n  lm(df.not_equiv)"
  lst$DC_SOLUTION <- "df.equiv <- data.frame(c = c(1, 2, 3), d = c(4, 5, 6))\n  var(df.equiv)\n  df.not_equiv <- data.frame(c = c(7, 8, 9), d = c(4, 5, 6))\n  lm(df.not_equiv)"

  lst$DC_SCT <- "test_function(\"var\", \"x\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", eval = FALSE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"lm\", eval = FALSE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", eval = FALSE, eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"lm\", eval = FALSE, eq_condition = \"equal\")"
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


test_that("test_function, more tests", {
  lst <- list()
  lst$DC_CODE <- "var.iden <- 3\n  var(var.iden)\n  var.equal <- 4\n  mean(var.equal)"
  lst$DC_SOLUTION <- "var.iden <- 3 + 4.4e-8\n  var(var.iden)\n  var.equal <- 4\n  mean(var.equal)"

  lst$DC_SCT <- "test_function(\"var\", \"x\", eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", \"x\", eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", \"x\", eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", eval = FALSE, eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", eval = FALSE, eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", eval = FALSE, eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", eval = FALSE, eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", \"x\", eq_condition = \"identical\")"
  output <- test_it(lst)
  fails(output)
})


test_that("test_function, eval", {
  lst <- list()
  lst$DC_CODE <- "var.a <- c(302, 305, 309)\n  mean(var.a)\n  var(var.a)"
  lst$DC_SOLUTION <- "var.b <- c(302, 305, 309)\n  mean(var.b)\n  var(var.b)"

  lst$DC_SCT <- "test_function(\"mean\", \"x\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", \"x\", eval = FALSE)"
  output <- test_it(lst)
  fails(output)

})

test_that("test_function, allow_extra", {
  lst <- list()
  lst$DC_CODE <- "mean(1:10, trim = 0.9)\n  var(1:5, 6:10)"
  lst$DC_SOLUTION <- "mean(1:10, trim = 0.9)\n  var(1:5, 6:10)"

  lst$DC_SCT <- "test_function(\"mean\", \"x\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", c(\"x\", \"trim\"), allow_extra = FALSE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", c(\"x\", \"y\"), allow_extra = FALSE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SOLUTION <- "mean(1:10)\n  var(1:5, 6:10)"
  lst$DC_SCT <- "test_function(\"mean\", \"x\", allow_extra = FALSE)"
  output <- test_it(lst)
  fails(output)
})


test_that("test_function, allow_extra and ignore", {
  lst <- list()
  lst$DC_CODE <- "mean(1:10, trim = 0.9, na.rm = FALSE)\n  var(1:5, 6:10)"
  lst$DC_SOLUTION <- "mean(1:10)\n  var(1:5, 11:15)"

  lst$DC_SCT <- "test_function(\"mean\", \"x\", allow_extra = FALSE, ignore = c(\"trim\", \"na.rm\"))"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"var\", \"x\", allow_extra = FALSE, ignore = \"y\")"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", \"x\", allow_extra = FALSE, ignore = \"na.rm\")"
  output <- test_it(lst)
  fails(output)

  lst$DC_SCT <- "test_function(\"mean\", \"x\", allow_extra = FALSE, ignore = \"na.rm\")"
  output <- test_it(lst)
  fails(output)
})


test_that("test_function, index stuff", {
  lst <- list()
  lst$DC_CODE <- "a <- \"test\"\n  mean(1:10, trim = 0.9, na.rm = FALSE)\n  mean(1:5, trim = 0.8)\n  mean(1:10, trim = 0.9)"
  lst$DC_SOLUTION <- "a <- \"test\"\n  mean(1:10, trim = 0.9)\n  mean(1:9)\n  mean(1:10)"

  lst$DC_SCT <- "test_function(\"mean\", \"x\", index = 1)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", c(\"x\", \"trim\"))"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", \"x\", index = 1)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", c(\"x\", \"trim\"), allow_extra = FALSE, index = 1)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function(\"mean\", \"x\", allow_extra = FALSE, index = 3)"
  output <- test_it(lst)
  fails(output)
})

test_that("test_function, a bit of everything", {
  lst <- list()
  lst$DC_CODE <- "mean(1:10, na.rm = FALSE)\nmean(1:10, trim = 0.1)"
  lst$DC_SOLUTION <- "mean(1:10, trim = 0.1)\nmean(1:10, na.rm = FALSE)"

  lst$DC_SCT <- "test_function('mean', args = 'x', index = 1)\ntest_function('mean', args = 'x', index = 2)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim'), index = 1)\ntest_function('mean', args = c('x', 'na.rm'), index = 2)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- paste0("test_function('mean', args = 'x', index = 2)\n",
                       "test_function('mean', args = 'na.rm', index = 2)")
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- paste0("test_function('mean', args = 'x', index = 1)\n",
                       "test_function('mean', args = 'trim', index = 1)")
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
  line_info(output, 5, 5)
})

test_that("test_function works with S3 functions", {
  lst <- list()
  lst$DC_PEC <- "
      set.seed(1)
      library(rpart)
      fit <- rpart(Kyphosis ~ Age + Number + Start, method='class', data=kyphosis)"
  lst$DC_CODE <- "predict(type = 'class', newdata = test, lm(c(1,2,3) ~ c(4,5,6)))
                  predict(object = fit, type = 'class', kyphosis)"
  lst$DC_SOLUTION <- "predict(object = fit, type = 'class', kyphosis)
                      predict(type = 'class', newdata = kyphosis, fit)"
  lst$DC_SCT <- "test_function('predict', args = c('object', 'type'), index = 1)"
  output <- test_it(lst)
  passes(output)

  lst <- list()
  lst$DC_SOLUTION <- "mean(c(1:10, NA), 0.1, TRUE)"
  lst$DC_CODE <- "mean(c(1:10, NA), 0.1, TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim', 'na.rm'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_function works appropriately inside test_corect", {
  lst <- list()
  lst$DC_SOLUTION <- "summary(mtcars)\nsummary(pressure)"
  lst$DC_CODE <- "summary(mtcars)\nsummary(cars)"
  lst$DC_SCT <- paste("test_correct(test_output_contains('summary(mtcars)'), test_function('summary', args = 'object', index = 1))",
                      "test_correct(test_output_contains('summary(pressure)'), test_function('summary', args = 'object', index = 2))", sep = "\n")

  output <- test_it(lst)
  fails(output)
  line_info(output, 2, 2)
})

test_that("test_function works with the pipe operator and summarise/summarize", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(hp))"
  lst$DC_SCT <- "test_function('summarise', args = '.data')"

  lst$DC_CODE <- "mtcars %>% summarise(avg = mean(hp))"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "mtcars %>% summarize(avg = mean(hp))"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "cars %>% summarize(avg = mean(speed))"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst$DC_CODE <- "mtcars %>% select(hp)"
  output <- test_it(lst)
  fails(output)
})

test_that("test_function works with incorrect_msg that's a vector", {
  lst <- list()
  lst$DC_SOLUTION <- "mean(1:20, trim = 0.1, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim', 'na.rm'), incorrect_msg = c('x_incorrect', 'trim_incorrect', 'na.rm_incorrect'))"

  lst$DC_CODE <- "mean(1:10, trim = 0.1, na.rm = TRUE)"
  output <- test_it(lst)
  fails(output, 'x_incorrect')

  lst$DC_CODE <- "mean(1:20, trim = 0.2, na.rm = TRUE)"
  output <- test_it(lst)
  fails(output, 'trim_incorrect')

  lst$DC_CODE <- "mean(1:20, trim = 0.1, na.rm = FALSE)"
  output <- test_it(lst)
  fails(output, 'na.rm_incorrect')

  # take the first one if multiple args wrong
  lst$DC_CODE <- "mean(1:10, trim = 0.2, na.rm = TRUE)"
  output <- test_it(lst)
  fails(output, 'x_incorrect')
})

test_that("test_function works with error in args", {
  lst <- list()
  lst$DC_SOLUTION <- "mean(1:20, trim = 0.1, na.rm = TRUE)"
  lst$DC_CODE <- "mean(1:20, trim = not_defined, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim', 'na.rm'))"
  output <- test_it(lst)
  fails(output)
})

test_that("test_function gives good automatic messages", {
  lst <- list()
  lst$DC_SOLUTION <- "mean(1:20, trim = 0.1, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim', 'na.rm'))"

  mess_patt1 <- "Did you correctly specify the argument <code>x</code> in your call"
  mess_patt2 <- "The object you specified has length 10, while it should have length 20"
  # match by pos
  lst$DC_CODE <- "mean(1:10,\ntrim = 0.1,\nna.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 1, 1)

  # match by name
  lst$DC_CODE <- "mean(x = 1:10,\ntrim = 0.1,\nna.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 1, 1)

  # match by name
  lst$DC_CODE <- "mean(trim = 0.1,\nx = 1:10,\nna.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 2, 2)

  # match by name
  lst$DC_CODE <- "mean(trim = 0.1,\nna.rm = TRUE,\nx = 1:10)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 3, 3)

  # two args wrong -> only mention the first
  lst$DC_CODE <- "mean(1:10,\ntrim = 0.2,\nna.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 1, 1)

  lst$DC_CODE <- "mean(x = 1:10,\ntrim = 0.2,\nna.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 1, 1)

  lst$DC_CODE <- "mean(trim = 0.2,\nx = 1:10,\nna.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 2, 2)

  lst$DC_CODE <- "mean(trim = 0.2,\nna.rm = TRUE,\nx = 1:10)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, mess_patt = mess_patt2)
  line_info(output, 3, 3)
})

test_that("test_function gives good automatic messages - deep", {
  lst <- list()
  lst$DC_SOLUTION <- "print('This is a serious thing!')"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  mess_patt1 <- "Did you correctly specify the argument <code>x</code> in your call of <code>print\\(\\)</code>"

  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "The object you specified is a number, while it should be a character string")

  lst$DC_CODE <- "print(c('this is', 'a serious thing'))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "The object you specified has length 2, while it should have length 1")

  lst$DC_CODE <- "print('this is a serious thing!')"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "Note that R is case-sensitive")

  lst$DC_CODE <- "print('This is a serious thingyyy!')"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "There might be a typo in there")
})


test_that("test_function gives good automatic messages - deep - 2", {
  lst <- list()
  lst$DC_SOLUTION <- "print(123)"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  mess_patt1 <- "Did you correctly specify the argument <code>x</code> in your call of <code>print\\(\\)</code>"

  lst$DC_CODE <- "print(c(T, F))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "The object you specified is a logical vector, while it should be a number")

  lst$DC_CODE <- "print(c(123, 123))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "The object you specified has length 2, while it should have length 1")

  lst$DC_CODE <- "print(c(a = 123))"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function('print', args = 'x', eq_condition = 'equal', index = 1)"
  lst$DC_CODE <- "print(c(a = 123))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "Are you sure the attributes")
})

test_that("try-errors", {
  lst <- list()
  lst$DC_PEC <- "RBackend::allow_solution_error()"
  lst$DC_SOLUTION <- "print(123 + 'test')"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  lst$DC_CODE <- "print(123 + 'test')"
  output <- test_it(lst)
  error(output, mess_patt = "There are arguments in the first function call of print\\(\\) that cause errors")
  
  lst <- list()
  lst$DC_SOLUTION <- "print(123)"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  lst$DC_CODE <- "print('test' + 123)"
  output <- test_it(lst)
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code> in your call of <code>print\\(\\)</code>")
  fails(output, "Evaluating the expression you specified caused an error")
})
