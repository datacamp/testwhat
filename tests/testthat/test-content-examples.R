context("content_examples")

test_that("exercise intermediate r (1)", {
  lst <- list()
  lst$DC_CODE <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SOLUTION <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SCT <- 'test_object("day1")'
  output <- test_it(lst)
  passes(output)
})

test_that("exercise intermediate r (2)", {
  lst <- list()
  lst$DC_PEC <- "linkedin <- c(16, 9, 13, 5, 2, 17, 14)"
  lst$DC_SOLUTION <- "for (li in linkedin) { print(li) }"
  lst$DC_CODE <- "for (li in linkedin) { print(li + 1) }"
  lst$DC_SCT <- "test_output_contains('invisible(lapply(linkedin,print))')"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("exercise ggplot2 - v1", {
  lst <- list()
  lst$DC_PEC <- "library(ggplot2)"
  lst$DC_SCT <- "
test_function_v2('qplot', 'data', index = 1)
test_function_v2('qplot', 'x', eval = FALSE, index = 1)
test_function_v2('qplot', 'data', index = 2)
test_function_v2('qplot', 'x', eval = FALSE, index = 2)
test_function_v2('qplot', 'y', eval = FALSE, index = 2)
test_function_v2('qplot', 'data', index = 3)
test_function_v2('qplot', 'x', eval = FALSE, index = 3)
test_function_v2('qplot', 'y', eval = FALSE, index = 3)
test_function_v2('qplot', 'geom', eval = FALSE, index = 3)
test_error()
success_msg('Good job!')
  "
  lst$DC_SOLUTION <- "
qplot(factor(cyl), data = mtcars)
qplot(factor(cyl), factor(vs), data = mtcars)
qplot(factor(cyl), factor(vs), data = mtcars, geom = 'jitter')
  "
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("exercise ggplot2 - v2", {
  lst <- list()
  lst$DC_PEC <- "library(ggplot2)"
  lst$DC_SCT <- "
  test_function_v2('qplot', args = c('data', 'x'), eval = c(T, F), index = 1)
  test_function_v2('qplot', c('data', 'x', 'y'), eval = c(T, F, F), index = 2)
  test_function_v2('qplot', c('data', 'x', 'y','geom'), eval = c(T, F, F, F), index = 3)
  test_error()
  "
  lst$DC_SOLUTION <- "
  qplot(factor(cyl), data = mtcars)
  qplot(factor(cyl), factor(vs), data = mtcars)
  qplot(factor(cyl), factor(vs), data = mtcars, geom = 'jitter')
  "
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

# # NOT FIXED!
# test_that("exercise cleaning data", {
#   lst <- list()
#   lst$DC_PEC <- "states <- c('a', 'b', 'c', 'd')"
#   lst$DC_CODE <- "states\nstates_upper <- toupper(states)\ntolower <- tolower(states_upper)"
#   lst$DC_SOLUTION <- "states\nstates_upper <- toupper(states)\ntolower(states_upper)"
#   lst$DC_SCT <- "test_function('tolower', 'x')"
#   output <- test_it(lst)
#   passes(output)
# })