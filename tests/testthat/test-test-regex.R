context("test_code")

# Note: the hashtags in the DC_CODE's below are to avoid any runtime errors 
# (they aren't a problem actually)

test_that("test_code - basic", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_code('a{2}')"

  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output, mess_patt = "the pattern <code>a\\{2\\}</code>")

  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  passes(output)
})

test_that("test_code - basic - custom", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_code('a{2}', missing_msg = 'missing')"

  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output, mess_patt = "Missing")

  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  passes(output)
})

test_that("test_code - different patterns", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_code(c('a{2}', 'b{2}'))"
  
  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "# bb"
  output <- test_it(lst)
  passes(output)
})

test_that("test_code - fixed", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_code('a{2}', fixed = TRUE)"
  
  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# a{2}"
  output <- test_it(lst)
  passes(output)
})

test_that("test_code - times", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_code('a{2}', times = 2)"
  
  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# aaaa"
  output <- test_it(lst)
  passes(output)  
})

test_that("test_code - different patterns + fixed + times", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_code(c('a{2}', 'b{2}'), times = 3, fixed = TRUE)"
  
  lst$DC_CODE <- "# aaaabb"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# aabbbb"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# bbbb"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# a{2} b{2}"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# a{2} b{2} b{2}"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "# a{2} a{2} b{2}"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "# a{2} b{2} b{2}"
  output <- test_it(lst)
  passes(output)
})

test_that("test_code - backwards compatible (test_student_typed)", {
  lst <- list()
  lst$DC_SCT <- "test_student_typed(c('a{2}', 'b{2}'), times = 3)" # fixed = TRUE by default!
  
  lst$DC_CODE <- "# aaaabb"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# aabbbb"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# bbbb"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# a{2} b{2}"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "# a{2} b{2} b{2}"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "# a{2} a{2} b{2}"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "# a{2} b{2} b{2}"
  output <- test_it(lst)
  passes(output)
})


context("test_output")

test_that("test_output_regex - basic", {
  lst <- list()
  lst$DC_ECHO <- TRUE
  lst$DC_CODE <- "'testing'\n123"

  lst$DC_SCT <- "ex() %>% test_output('123')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% test_output('\\\\d{3,}')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% test_output('\\\\d{4,}')"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% test_output('\\\\d{4,}', missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')

  lst$DC_SCT <- "ex() %>% test_output('123', fixed = TRUE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% test_output('1234', fixed = TRUE)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% test_output('1234', fixed = TRUE, missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')

  lst$DC_SCT <- "ex() %>% test_output('testing', times = 2)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% test_output('\\\\[1\\\\] testing', fixed = TRUE, times = 2)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% test_output('testing', times = 2, missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')
})

test_that("test_output_regex - errors", {
  lst <- list()
  lst$DC_ECHO <- TRUE

  # below code gives error: Error: non-numeric argument to binary operator
  lst$DC_CODE <- "'test' + 3"

  lst$DC_SCT <- "ex() %>% test_output('non-numeric')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% test_output('argument', times = 2)"
  output <- test_it(lst)
  fails(output)

  lst$DC_SCT <- "ex() %>% test_output('argument', times = 2, missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')
})


context("test_output_expr")

test_that("test_output_expr - basic", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_output_expr('print(123)')"
  
  lst$DC_CODE <- "print(12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Is the output of <code>print\\(123\\)</code> in your script")
  
  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_output_expr - basic - custom", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_output_expr('print(123)', missing_msg = 'missing')"
  
  lst$DC_CODE <- "print(12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Missing")
  
  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_output_expr - times", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% test_output_expr('print(123)', times = 3)"
  
  lst$DC_CODE <- "print(123); print(123)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "print(123); print(123); print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_output_expr - error", {
  lst <- list()
  lst$DC_CODE <- "x <- mtcars\nsummary(x)\nrm(x)"
  lst$DC_SCT <- "ex() %>% test_output_expr(\'summary(x)\')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_output_expr - backwards compatatibility", {
  lst <- list()
  lst$DC_SCT <- "test_output_contains('print(123)')"
  
  lst$DC_CODE <- "print(12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Is the output of <code>print\\(123\\)</code> in your script")
  
  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  passes(output)
})
