context("test_call_result - functions")

test_that("test call result - functions - step by step", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "ex() %>% test_fun('summarise') %>% test_result() %>% test_equal()"

  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>summarise\\(\\)</code>")

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>summarise\\(\\)</code>")
  fails(output, mess_patt = "Running it again threw an error.")

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>summarise\\(\\)</code>")
  fails(output, mess_patt = "Running it again doesn")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test call result - functions - step by step - custom", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "ex() %>% test_fun('summarise', not_called_msg = 'notcalled') %>% test_result(error_msg = 'error') %>% test_equal(incorrect_msg = 'incorrect')"

  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output, mess_patt = "Notcalled")

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>summarise\\(\\)</code>")
  fails(output, mess_patt = "Error")

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>summarise\\(\\)</code>")
  fails(output, mess_patt = "Running it again doesn")
  fails(output, mess_patt = "Incorrect")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test call result - functions - backwards compatbile", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "test_function_result('summarise')"

  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>summarise\\(\\)</code>?")

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>summarise\\(\\)</code>")
  fails(output, mess_patt = "Running it again threw an error.")

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>summarise\\(\\)</code>")
  fails(output, mess_patt = "Running it again doesn")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test call result - functions - errs appropriately", {
  # TODO
})

test_that("test call result - functions - indexing", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))\nmtcars %>% summarise(min = min(mpg), sd = sd(mpg))"
  lst$DC_SCT <- "ex() %>% test_fun('summarise', index = 1) %>% test_result() %>% test_equal()
                 ex() %>% test_fun('summarise', index = 2) %>% test_result() %>% test_equal()"

  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>summarise\\(\\)</code>")

  lst$DC_CODE <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>summarise\\(\\)</code> twice")

  lst$DC_CODE <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))\nmtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  line_info(output, 2, 2)

  lst$DC_CODE <- "mtcars %>% summarise(max = max(mpg), max2 = max(mpg))\nmtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst$DC_CODE <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))\nmtcars %>% summarise(min = min(mpg), sd = sd(mpg))"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), sd = sd(mpg))\nmtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  output <- test_it(lst)
  passes(output)
})


context("test_call_result - operators")

test_that("test_op - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "ex() %>% test_op('+') %>% test_result() %>% test_equal()"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Have you used the <code>\\+</code> operator")

  lst$DC_CODE <- "4 + 'a'"
  output <- test_it(lst)
  fails(output, mess_patt = "Running the operation again threw an error")

  lst$DC_CODE <- "7 + 9"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you correctly used the <code>\\+</code> operator")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test_op - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "ex() %>% test_op('+', not_called_msg = 'notcalled') %>% test_result(error_msg = 'error') %>% test_equal(incorrect_msg = 'incorrect')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Notcalled")

  lst$DC_CODE <- "4 + 'a'"
  output <- test_it(lst)
  fails(output, mess_patt = "Error")

  lst$DC_CODE <- "7 + 9"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you correctly used the <code>\\+</code> operator?")
  fails(output, mess_patt = "Incorrect")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


test_that("arithmetic operators and logical operators:", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "test_operator('+')"

  lst$DC_CODE <- "7 + 8"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "1 + 3\n7 + 8"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "6 + 9"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "1 + 3\n6 + 9"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "mean(7 + 8)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "7 + 6"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "1 + 3\n7 + 6"
  output <- test_it(lst)
  fails(output)

  # mathematical operators: + - % * %% ^
  lst <- list()
  lst$DC_CODE <- "1 + 1\n8 - 3\n2 * 2\n3 / 4\n4 ^ 4\n56 %% 43\n"
  lst$DC_SOLUTION <- "1 + 1\n8 - 3\n2 * 2\n3 / 4\n4 ^ 4\n56 %% 43\n"
  lst$DC_SCT <- paste("test_operator('+')", "test_operator('-')",
                      "test_operator('/')", "test_operator('*')",
                      "test_operator('^')", "test_operator('%%')", sep = "\n")
  output <- test_it(lst)
  passes(output)

  # relational operators:  == != < > <= >=
  lst <- list()
  lst$DC_CODE <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\n4 == 4\n5 != 7"
  lst$DC_SOLUTION <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\n4 == 4\n5 != 7"
  lst$DC_SCT <- paste("test_operator('<')", "test_operator('>')",
                      "test_operator('<=')", "test_operator('>=')",
                      "test_operator('==')", "test_operator('!=')", sep = "\n")
  output <- test_it(lst)
  passes(output)

  # logical operators: & | && || !
  lst <- list()
  lst$DC_CODE <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\nc(T, T) & c(F, F)\nTRUE && (4 < 3)\nc(T, T) | (4 < 3)\nTRUE || FALSE\n !TRUE"
  lst$DC_SOLUTION <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\nc(T, T) & c(F, F)\nTRUE && (4 < 3)\nc(T, T) | (4 < 3)\nTRUE || FALSE\n !TRUE"
  lst$DC_SCT <- paste("test_operator('&')", "test_operator('|')",
                      "test_operator('&&')", "test_operator('||')",
                      "test_operator('!')", sep = "\n")
  output <- test_it(lst)
  passes(output)
})

test_that("test_operator messaging", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"

  lst$DC_CODE <- ""
  lst$DC_SCT <- "test_operator('+')"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you used the <code>\\+</code> operator")

  lst$DC_SCT <- "test_operator('+', not_called_msg = 'test')"
  output <- test_it(lst)
  fails(output, mess_patt = "Test")

  lst$DC_CODE <- "4 + 5"
  lst$DC_SCT <- "test_operator('+')"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you correctly used")
  line_info(output, 1, 1)

  lst$DC_SCT <- "test_operator('+', incorrect_msg = 'wrong')"
  output <- test_it(lst)
  fails(output, mess_patt = "Wrong")
  line_info(output, 1, 1)
})

test_that("test_operator with eval", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_CODE <- "4 + 5"

  lst$DC_SCT <- "test_operator('+')"
  output <- test_it(lst)
  fails(output)

  lst$DC_SCT <- "test_operator('+', eval = FALSE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_operator with indexes", {
  lst <- list()
  lst$DC_SOLUTION <- "4 + 5\n10 + 20"
  lst$DC_SCT <- paste("test_operator('+', index = 1, not_called_msg = 'ncm1', incorrect_msg = 'icm1')",
                      "test_operator('+', index = 2, not_called_msg = 'ncm2', incorrect_msg = 'icm2')", sep = "\n")

  lst$DC_CODE <- "4 + 5\n10 + 20"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "10 + 20\n4 + 5"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = 'Ncm1')

  lst$DC_CODE <- "4 + 5\n4 + 5"
  output <- test_it(lst)
  fails(output, mess_patt = 'Icm2')
  line_info(output, 2, 2)

  lst$DC_CODE <- "4 + 5"
  output <- test_it(lst)
  fails(output, mess_patt = 'Ncm2')

  lst$DC_CODE <- "10 + 20"
  output <- test_it(lst)
  fails(output, mess_patt = 'Icm1')
  line_info(output, 1, 1)
})

test_that("test_operator errs correctly", {
  lst <- list()
  lst$DC_SCT <- "test_operator('+')"

  lst$DC_SOLUTION <- ""
  output <- test_it(lst)
  error(output)
})


