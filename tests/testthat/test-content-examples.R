context("content_examples")
source("helpers.R")

test_that("exercise intermediate R", {
  lst <- list()
  lst$DC_CODE <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SOLUTION <- "today <- Sys.Date()\nday1 <- today - 11"
  lst$DC_SCT <- 'test_object("day1")'
  output <- test_it(lst)
  passes(output)
})

test_that("exercise datatable", {
  lst <- list()
  lst$DC_PEC <- "library(data.table)\nlibrary(RdatatableSCT)"
  lst$DC_CODE <- "DT <- data.table(A = letters[c(1, 1, 1, 2, 2)], B = 1:5)\nDT[, Total := sum(B), by = A]"
  lst$DC_SOLUTION <- lst$DC_CODE
  lst$DC_SCT <- '
msg <- "Do not change the code that builds the initial data.table `DT`."
test_function("data.table", args = c("A", "B"), not_called_msg = msg, incorrect_msg = msg)
test_object("DT", eval = FALSE)
result_one <- test_call(table_name = "DT",
                        student_code = paste0("DT", get_bracket_code("DT",get_student_code())[1]),
                        correct_call = "DT[,Total:=sum(B),by=A]",
                        begin_dt = data.table(A=letters[c(1,1,1,2,2)], B=1:5),
                        elements = c("i","j","by"))
i_msg <- "In this exercise you do not need to worry about taking a specific subset of rows hence the `i` part of the data.table call should be empty!"
test_what(expect_true(called_brackets_on("DT") > 0), feedback = "Get started by reading the instructions.")
test_what(expect_false(is.null(get_bracket_code("DT",get_student_code())[1])), feedback = "Have another look at the first instruction.")
test_what(expect_true(result_one["i"]), feedback = i_msg)
test_what(expect_true(result_one["j"]), feedback = "Use the `:=` operator you just learned to add a column total by reference containing sum(B).")
test_what(expect_true(result_one["by"]), feedback = "Add a column total by reference containing sum(B) for EACH group in column A.")
  '
  output <- test_it(lst)
  passes(output)
})

test_that("exercise intermediate r", {
  lst <- list()
  lst$DC_PEC <- 'load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_753/datasets/chapter2.RData"))'
  lst$DC_CODE <- 'str(logs)\nlogs[[11]]$detaidls\nclass(logs[[1]]$timestamp)'
  lst$DC_SOLUTION <- lst$DC_CODE
  lst$DC_SCT <- 'test_function("class", "x", incorrect_msg = "Have you passed the `timestamp` component of `logs[[1]]` to `class()`?")'
  output <- test_it(lst)
  passes(output)
})

test_that("exercise cleaning data", {
  lst <- list()
  lst$DC_PEC <- "states <- c('a', 'b', 'c', 'd')"
  lst$DC_CODE <- "states\nstates_upper <- toupper(states)\ntolower <- tolower(states_upper)"
  lst$DC_SOLUTION <- "states\nstates_upper <- toupper(states)\ntolower(states_upper)"
  lst$DC_SCT <- "test_function('tolower', 'x')"
  output <- test_it(lst)
  passes(output)
})