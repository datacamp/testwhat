context("test_function")

test_that("test_function step by step", {
  lst <- list(DC_SOLUTION = "mean(1:3, na.rm = TRUE)",
              DC_SCT = "test_function('mean', args = c('x', 'na.rm'))")

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "mean(1:3)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>\\. Did you specify the argument <code>na.rm</code>?")

  lst$DC_CODE <- "mean(1:3, na.rm = FALSE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>\\. Did you correctly specify the argument <code>na.rm</code>?")

  lst$DC_CODE <- "mean(1:3, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_function step by step - custom", {
  lst <- list(DC_SOLUTION = "mean(1:3, na.rm = TRUE)",
              DC_SCT = "test_function('mean', args = c('x', 'na.rm'), not_called_msg = 'notcalled', args_not_specified_msg = 'notspecified', incorrect_msg = 'incorrect')")

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Notcalled")

  lst$DC_CODE <- "mean(1:3)"
  output <- test_it(lst)
  fails(output, mess_patt = "Notspecified")

  lst$DC_CODE <- "mean(1:3, na.rm = FALSE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")

  lst$DC_CODE <- "mean(1:3, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_function step by step - custom - 2", {
  lst <- list(DC_SOLUTION = "mean(1:3, na.rm = TRUE)",
              DC_SCT = "test_function('mean', args = c('x', 'na.rm'), not_called_msg = 'notcalled', args_not_specified_msg = 'notspecified', incorrect_msg = c('incorrect1', 'incorrect2'))")

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Notcalled")

  lst$DC_CODE <- "mean(1:3)"
  output <- test_it(lst)
  fails(output, mess_patt = "Notspecified")

  lst$DC_CODE <- "mean(1:2, na.rm = FALSE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect1")

  lst$DC_CODE <- "mean(1:3, na.rm = FALSE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect2")

  lst$DC_CODE <- "mean(1:3, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_function - index (1)", {
  lst <- list()
  lst$DC_SOLUTION <- "mean(1:10, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'na.rm'), index = 1)"

  lst$DC_CODE <- "mean(1:10, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "mean(1:10, na.rm = FALSE)"
  output <- test_it(lst)
  line_info(output, 1, 1, 20, 24)
  fails(output)

  lst$DC_CODE <- "mean(1:10, na.rm = FALSE)\nmean(1:10, na.rm = FALSE)"
  output <- test_it(lst)
  line_info(output, 1, 1, 20, 24)
  fails(output)

  lst$DC_CODE <- "mean(1:10, na.rm = FALSE)\nmean(1:10, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_function - index (2)", {
  lst <- list()
  lst$DC_SOLUTION <- "mean(1:10, na.rm = TRUE)\nmean(1:10)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'na.rm'), index = 1)\ntest_function('mean', args = 'x', index = 2)"

  lst$DC_CODE <- "mean(1:10, na.rm = TRUE)\nmean(1:10)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "mean(1:10)\nmean(1:10, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "mean(1:10, na.rm = TRUE)"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "mean(1:10, na.rm = TRUE)\nmean(1:8)"
  output <- test_it(lst)
  fails(output)
  line_info(output, 2, 2, 6, 8)
})

test_that("test_function - index (3)", {
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

  lst$DC_SCT <- paste('test_function("sub", "pattern", index = 1)\n',
                      'test_function("sub", "pattern", index = 2)\n',
                      'test_function("sub", "replacement", index = 1)\n',
                      'test_function("sub", "replacement", index = 2)\n',
                      'test_function("sub", "x", index = 1)\n',
                      'test_function("sub", "x", index = 2)')
  output <- test_it(lst)
  passes(output)
})

test_that("test_function - eq_condition", {
  lst <- list()
  lst$DC_CODE <- "df.equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))\n  var(df.equiv)\n  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))\n  lm(df.not_equiv)"
  lst$DC_SOLUTION <- "df.equiv <- data.frame(c = c(1, 2, 3), d = c(4, 5, 6))\n  var(df.equiv)\n  df.not_equiv <- data.frame(c = c(7, 8, 9), d = c(4, 5, 6))\n  lm(df.not_equiv)"

  lst$DC_SCT <- "test_function('var', 'x')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function('lm', 'formula')"
  output <- test_it(lst)
  fails(output)

  lst$DC_SCT <- "test_function('var', 'x', eq_condition = 'equal')"
  output <- test_it(lst)
  fails(output)

  lst$DC_SCT <- "test_function('lm', 'formula', eq_condition = 'equal')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_function - eval", {
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

# test_that("test_function errs correctly", {})

test_that("test_function - diff messages - 1", {
  lst <- list()
  lst$DC_SOLUTION <- "mean(1:20, trim = 0.1, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('mean', args = c('x', 'trim', 'na.rm'))"

  mess_patt1 <- "Check your call of <code>mean\\(\\)</code>\\. Did you correctly specify the argument <code>x</code>?"
  mess_patt2 <- "It has length 10, while it should have length 20"

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

test_that("test_function - diff messages - 2", {
  lst <- list()
  lst$DC_SOLUTION <- "print('This is a serious thing!')"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  mess_patt1 <- "Check your call of <code>print\\(\\)</code>\\. Did you correctly specify the argument <code>x</code>?"

  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "It is a number, while it should be a character string")

  lst$DC_CODE <- "print(c('this is', 'a serious thing'))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "It has length 2, while it should have length 1")

  lst$DC_CODE <- "print('this is a serious thing!')"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "Note that R is case-sensitive")

  lst$DC_CODE <- "print('This is a serious thingyyy!')"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "There might be a typo in there")
})

test_that("test_function - diff messages - 3", {
  lst <- list()
  lst$DC_SOLUTION <- "print(123)"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  mess_patt1 <- "Check your call of <code>print\\(\\)</code>\\. Did you correctly specify the argument <code>x</code>?"

  lst$DC_CODE <- "print(c(T, F))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "It is a logical vector, while it should be a number")

  lst$DC_CODE <- "print(c(123, 123))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "It has length 2, while it should have length 1")

  lst$DC_CODE <- "print(c(a = 123))"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_function('print', args = 'x', eq_condition = 'equal', index = 1)"
  lst$DC_CODE <- "print(c(a = 123))"
  output <- test_it(lst)
  fails(output, mess_patt = mess_patt1)
  fails(output, "Are you sure the attributes")
})

test_that("test_function - diff messages - try-errors", {
  lst <- list()
  lst$DC_PEC <- "RBackend::allow_solution_error()"
  lst$DC_SOLUTION <- "print(123 + 'test')"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  lst$DC_CODE <- "print(123 + 'test')"
  output <- test_it(lst)
  error(output, mess_patt = "test_equal\\(\\) found an argument that causes an error when evaluated")

  lst <- list()
  lst$DC_SOLUTION <- "print(123)"
  lst$DC_SCT <- "test_function('print', args = 'x', index = 1)"
  lst$DC_CODE <- "print('test' + 123)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>\\. Did you correctly specify the argument <code>x</code>?")
  fails(output, "Evaluating the expression you specified caused an error")
})

test_that("test_function - S3 functions", {
  lst <- list()
  lst$DC_PEC <- "set.seed(1)\nlibrary(rpart)\nfit <- rpart(Kyphosis ~ Age + Number + Start, method='class', data=kyphosis)"
  lst$DC_CODE <- "predict(type = 'class', newdata = test, lm(c(1,2,3) ~ c(4,5,6)))\npredict(object = fit, type = 'class', kyphosis)"
  lst$DC_SOLUTION <- "predict(object = fit, type = 'class', kyphosis)\npredict(type = 'class', newdata = kyphosis, fit)"
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

test_that("test_function - piped operators", {
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

test_that("test_function - formulas", {
  lst <- list()
  lst$DC_SOLUTION <- "lm(mpg ~ wt + hp, data = mtcars)"
  lst$DC_SCT <- "test_function('lm', args = 'formula')"

  lst$DC_CODE <- "lm(mpg ~ wt + hp, data = mtcars)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "lm(mpg ~ hp + wt, data = mtcars)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "lm(mpg ~ wt + hp + drat, data = mtcars)"
  output <- test_it(lst)
  fails(output)

  lst <- list()
  lst$DC_SOLUTION <- "lm(mpg ~ ., data = mtcars)"
  lst$DC_CODE <- "lm(mpg ~ ., data = mtcars)"
  lst$DC_SCT <- "test_function('lm', args = 'formula')"
  output <- test_it(lst)
  passes(output)
})

test_that("test_function - ...", {
  lst <- list()
  lst$DC_SOLUTION <- "sum(1, 2, 3, 4, NA, na.rm = TRUE)"
  lst$DC_SCT <- "test_function('sum', args = c('...', 'na.rm'))"

  lst$DC_CODE <- "sum(1, 2, 3, 4, NA, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "sum(1, 1 + 1, 1 + 1 + 1, 4, NA, na.rm = TRUE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "sum(1, 2, 3, 4, NA, na.rm = FALSE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>sum\\(\\)</code>\\. Did you correctly specify the argument <code>na\\.rm</code>")

  lst$DC_CODE <- "sum(1, 2, 3, NA, na.rm = TRUE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Did you correctly specify the arguments that are matched to <code>...</code>")

  lst <- list()
  lst$DC_SOLUTION <- "sum(1, 2, 3, 4, 5)"
  lst$DC_SCT <- "test_function('sum', args = '...')"

  lst$DC_CODE <- "sum(1, 2, 3, 4, 5)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "sum(1, 2, 3, 4)"
  output <- test_it(lst)
  fails(output, mess_patt = "Did you correctly specify the arguments that are matched to <code>...</code>")
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



