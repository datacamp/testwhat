context("rstudio-test-props")

test_that("test set basic 1", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg)"
  lst$DC_SCT <- "test_props(index = 1, funs = 'ggvis')"
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~hp)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~hp) %>% layer_points()"
  output <- test_it(lst)
  fails(output)
})

test_that("test props argument", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg)"
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis'), props = c('x', 'y'))"
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~hp)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~hp) %>% layer_points()"
  output <- test_it(lst)
  fails(output)
})

test_that("test set basic different functions", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'))"
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis() %>% layer_points(~disp, ~mpg)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~hp) %>% layer_points()"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis() %>% layer_points(~disp, ~hp)"
  output <- test_it(lst)
  fails(output)
})

test_that("test set basic allow_extra", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg, size := 80) %>% layer_points()"
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis'))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis'), allow_extra = FALSE)"
  output <- test_it(lst)
  fails(output, mess_patt = "Do not define any other")
})


test_that("test not enough calls", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_CODE <- ""
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The system wants to test if the first command you entered")
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'), incorrect_number_of_calls_msg = 'silly')"
  output <- test_it(lst)
  fails(output, mess_patt = "silly")
})

test_that("test missing function calls", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg)"
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'))"
  output <- test_it(lst)
  fails(output, mess_patt = "Command 1 of your solution should")
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'), not_called_msg = 'silly')"
  output <- test_it(lst)
  fails(output, mess_patt = "silly")
})

test_that("test with different calls", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "mtcars %>% ggvis(~disp, ~mpg)\nmtcars %>% ggvis(~wt, ~hp)"
  lst$DC_SCT <- "test_props(1, 'ggvis')\ntest_props(2, 'ggvis')"
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg)\nmtcars %>% ggvis(~wt, ~hp)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "mtcars %>% ggvis(~disp, ~mpg)\nmtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  fails(output)
})


