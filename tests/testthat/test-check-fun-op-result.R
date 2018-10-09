context("check fun result")

test_that("test call result - functions - step by step", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "ex() %>% check_function('summarise') %>% check_result() %>% check_equal()"
  
  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Have you called <code>summarise()</code>")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Running it again threw an error.")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Running it again doesn")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test call result - functions - step by step - custom", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "ex() %>% check_function('summarise', not_called_msg = 'notcalled') %>% check_result(error_msg = 'error') %>% check_equal(incorrect_msg = 'incorrect')"
  
  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, mess_patt = "Notcalled")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Error")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Running it again doesn")
  fb_contains(output, "Incorrect")
  
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
  fails(output)
  fb_contains(output, "Have you called <code>summarise()</code>?")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Running it again threw an error.")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Running it again doesn&#39;t give the correct result.")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test call result - functions - backwards compatbile - custom", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "test_function_result('summarise', not_called_msg = 'notcalled', error_msg = 'error', incorrect_msg = 'incorrect')"
  
  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Notcalled")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  output <- test_it(lst)
  fails(output)
  fb_excludes(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Error")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  fb_excludes(output, "Check your call of <code>summarise()</code>")
  fb_contains(output, "Incorrect")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test call result - functions - errs appropriately", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "test_function_result('mutate')"
  
  expect_error(test_it(lst))
})

test_that("test call result - functions - indexing", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))\nmtcars %>% summarise(min = min(mpg), sd = sd(mpg))"
  lst$DC_SCT <- "ex() %>% check_function('summarise', index = 1) %>% check_result() %>% check_equal()
  ex() %>% check_function('summarise', index = 2) %>% check_result() %>% check_equal()"
  
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
  
  # no blacklisting anymore, so this shouldn't pass
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), sd = sd(mpg))\nmtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
})

test_that("test call result - custom eq_fun", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function() return(list(a = 1))\nmy_fun()"
  lst$DC_SCT <- "ex() %>% check_function('my_fun') %>% check_result() %>% check_equal(eq_fun = function(x, y) { x$a == y$a })"
  
  # correct
  exs <- list(
    list(code = "my_fun <- function() return(list(a = 1, b = 2))\nmy_fun()", correct = TRUE),
    list(code = "my_fun <- function() return(list(a = 2))\nmy_fun()", correct = FALSE),
    list(code = "my_fun <- function() return(1))\nmy_fun()", correct = FALSE)
  )
  
  for (ex in exs) {
    lst$DC_CODE <- ex$code
    output <- test_it(c(lst, DC_CODE = ex$code))
    if (ex$correct) passes(output) else fails(output)
  }
})

context("check operator result")

test_that("check_operator - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "ex() %>% check_operator('+') %>% check_result() %>% check_equal()"
  
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

test_that("check_operator - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "ex() %>% check_operator('+', not_called_msg = 'notcalled') %>% check_result(error_msg = 'error') %>% check_equal(incorrect_msg = 'incorrect')"
  
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


test_that("check_operator - arithmetic/relational/logical", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "ex() %>% check_operator('+') %>% check_result() %>% check_equal()"
  
  lst$DC_CODE <- "7 + 8"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "6 + 9"
  output <- test_it(lst)
  passes(output)
  
  # no blacklisting anymore, this fails
  lst$DC_CODE <- "1 + 3\n7 + 8"
  output <- test_it(lst)
  fails(output)
  
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
  lst$DC_SCT <- paste0("ex() %>% check_operator('",c("+", "-", "/", "*", "^", "%%"),"') %>% check_result() %>% check_equal()", collapse = "\n")
  output <- test_it(lst)
  passes(output)
  
  # relational operators:  == != < > <= >=
  lst <- list()
  lst$DC_CODE <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\n4 == 4\n5 != 7"
  lst$DC_SOLUTION <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\n4 == 4\n5 != 7"
  lst$DC_SCT <- paste0("ex() %>% check_operator('",c("<", ">", "<=", ">=", "==", "!="),"') %>% check_result() %>% check_equal()", collapse = "\n")
  output <- test_it(lst)
  passes(output)
  
  # logical operators: & | && || !
  lst <- list()
  lst$DC_CODE <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\nc(T, T) & c(F, F)\nTRUE && (4 < 3)\nc(T, T) | (4 < 3)\nTRUE || FALSE\n !TRUE"
  lst$DC_SOLUTION <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\nc(T, T) & c(F, F)\nTRUE && (4 < 3)\nc(T, T) | (4 < 3)\nTRUE || FALSE\n !TRUE"
  lst$DC_SCT <- paste0("ex() %>% check_operator('",c("&", "|", "&&", "||", "!"),"') %>% check_result() %>% check_equal()", collapse = "\n")
  output <- test_it(lst)
  passes(output)
})

test_that("check_operator - index", {
  lst <- list()
  lst$DC_SOLUTION <- "4 + 5\n10 + 20"
  lst$DC_SCT <- paste("ex() %>% check_operator('+', index = 1, not_called_msg = 'ncm1') %>% check_result() %>% check_equal(incorrect_msg = 'icm1')",
                      "ex() %>% check_operator('+', index = 2, not_called_msg = 'ncm2') %>% check_result() %>% check_equal(incorrect_msg = 'icm2')", sep = "\n")
  
  lst$DC_CODE <- "4 + 5\n10 + 20"
  output <- test_it(lst)
  passes(output)
  
  # no blacklisting, this should fail
  lst$DC_CODE <- "10 + 20\n4 + 5"
  output <- test_it(lst)
  fails(output)
  
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

test_that("check_operator - errs correctly", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_operator('+')"
  
  lst$DC_SOLUTION <- ""
  expect_error(test_it(lst))
})
