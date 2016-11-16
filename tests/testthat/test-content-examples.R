context("content_examples")

test_that("exercise intermediate R", {
  lst <- list()
  lst$DC_CODE <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SOLUTION <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SCT <- 'test_object("day1")'
  output <- test_it(lst)
  passes(output)
})

test_that("exercise intermediate r", {
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

test_that("exercise intermediate r - 2", {
  lst <- list()
  lst$DC_PEC <- 'load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_753/datasets/chapter2.RData"))'
  lst$DC_CODE <- 'str(logs)\nlogs[[11]]$detaidls\nclass(logs[[1]]$timestamp)'
  lst$DC_SOLUTION <- lst$DC_CODE
  lst$DC_SCT <- 'test_function("class", "x", incorrect_msg = "Have you passed the `timestamp` component of `logs[[1]]` to `class()`?")'
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
  success_msg('Good job!')
  "
  lst$DC_SOLUTION <- "
  # qplot() with x only
  qplot(factor(cyl), data = mtcars)

  # qplot() with x and y
  qplot(factor(cyl), factor(vs), data = mtcars)

  # qplot() with geom set to jitter manually
  qplot(factor(cyl), factor(vs), data = mtcars, geom = 'jitter')
  "
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("R for sas, spss, stata", {
  lst <- list()
  lst$DC_PEC <- "
load(url(\"http://s3.amazonaws.com/assets.datacamp.com/course/Bob/mydata100.RData\"))
pretest <- mydata100$pretest
gender <- mydata100$gender
  "
  lst$DC_SOLUTION <- "
by(pretest,
   gender,
   function(x){ c(mean(x, na.rm = TRUE),
                sd(x, na.rm = TRUE),
                median(x = x, na.rm = TRUE)) })
  "
  lst$DC_CODE <- lst$DC_SOLUTION
  lst$DC_SCT <- '
test_error()
test_function("by", "data",
  incorrect_msg = "There is something wrong with your data argument in the <code>by()</code> function.",
  not_called_msg = "Use the <code>by()</code> function with the data specified as first argument.")
  test_function("by", "INDICES",
  incorrect_msg = "There is something wrong with the grouping of your <code>by()</code> function.",
  not_called_msg = "Use the <code>by()</code> function with <code>gender</code> as grouping factor.")
  test_function("by", "FUN",
  incorrect_msg = "There is something wrong with the anonymous function in the <code>by()</code> function.",
  not_called_msg = "Use the <code>by()</code> function with the anonymous that needs to be applied.")
  '
  output <- test_it(lst)
  passes(output)
})

test_that("intermediate r practice", {
  lst <- list()
  lst$DC_PEC <- 'load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_753/datasets/chapter2.RData"))'
  lst$DC_SCT <- '
test_function("str", "object")
#test_output_contains("logs[[11]]$details")
#test_function("class", "x")
success_msg("AWESOME!")
  '
  lst$DC_SOLUTION <- "
str(logs)
logs[[11]]$details
class(logs[[1]])
  "
  lst$DC_CODE <- "
print(logs)
print(logs[[11]])
print(str(logs[[1]]$timestamp))
  "
  output <- test_it(lst)
  fails(output)
})

test_that("exercise eda", {
  lst <- list()
  lst$DC_PEC <- 'source("http://s3.amazonaws.com/assets.datacamp.com/production/course_1414/datasets/shared.R")
votes_joined <- read_dataset("votes_joined")'
  lst$DC_CODE <- '# Load the tidyr package
library(tidyr)
  
  # Gather the six mu/nu/di/hr/co/ec columns
  votes_joined %>%
  gather(topic, has_topic, me:ec)
  
  # Perform gather again, then filter
  votes_gathered <- votes_joined %>%
  gather(topic, has_topic, me:ec) %>%
  filter(has_topic == 1)'
  lst$DC_SOLUTION <- lst$DC_CODE
  lst$DC_SCT <- '
test_library_function("tidyr")

test_function("gather", args = c("data", "key", "value"), eval = c(T, F, F))

test_correct({
test_data_frame("votes_gathered", incorrect_msg = "Did you gather the six columns (`mu`, `nu`, `di`, `hr`, `co`, and `ec`) and filter such that `has_topic == 1`?")
}, {
test_function("gather", args = c("data", "key", "value"), eval = c(T, F, F))
test_function("filter")
})

test_error()
success_msg("Awesome job!")
  '
  
  output <- test_it(lst)
  passes(output)
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