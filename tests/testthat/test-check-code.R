context("check_code")

# Note: the hashtags in the DC_CODE's below are to avoid any runtime errors 
# (they aren't a problem actually)

test_that("check_code - basic", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code('a{2}')"

  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output, mess_patt = "the pattern <code>a\\{2\\}</code>")

  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  passes(output)
})

test_that("check_code - basic - custom", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code('a{2}', missing_msg = 'missing')"

  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output, mess_patt = "Missing")

  lst$DC_CODE <- "# aa"
  output <- test_it(lst)
  passes(output)
})

test_that("check_code - different patterns", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code(c('a{2}', 'b{2}'))"
  
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

test_that("check_code - fixed", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code('a{2}', fixed = TRUE)"

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

test_that("check_code - times", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code('a{2}', times = 2)"

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

test_that("check_code - drop_comments", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code('a', drop_comments = TRUE)"
  
  lst$DC_CODE <- "# a"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "a <- 1"
  output <- test_it(lst)
  passes(output)
})


test_that("check_code - different patterns + fixed + times", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_code(c('a{2}', 'b{2}'), times = 3, fixed = TRUE)"

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

test_that("check_code - backwards compatible (test_student_typed)", {
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

test_that("check_code - test_student_typed doesn't append", {
  lst <- list()
  lst$DC_SOLUTION <- "if (TRUE) { print('hello') }"
  lst$DC_CODE <- "if (TRUE) { print('goodnight') }"

  lst$DC_SCT <- "test_if_else(if_expr_test = test_student_typed('hello'))"
  output <- test_it(lst)
  fails(output)
  fb_contains(output, "Check the first if statement.")
  fb_contains(output, "Check the if part.")
  fb_contains(output, "Have you typed <code>hello</code>?")

  lst$DC_SCT <- "test_if_else(if_expr_test = test_student_typed('hello', not_typed_msg = 'nottyped'))"
  output <- test_it(lst)
  fails(output)
  fb_excludes(output, "Check the first if statement.")
  fb_excludes(output, "Check the if part.")
  fb_contains(output, "Nottyped")
})