context("rstudio_test_props")

test_that("test set basic 1", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  lst$DC_SCT <- "test_props(index = 1, funs = 'ggvis')"
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~hp)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~hp) %>% layer_points()"
  output <- test_it(lst)
  fails(output)
})

test_that("robust against incorrect calls", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)\nRBackend::allow_solution_error()"
  lst$DC_SCT <- "test_props(index = 1, funs = 'ggvis')"

  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg, ~wt)"
  lst$DC_CODE <- ""
  expect_error(test_it(lst))

  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg, ~wt)"
  output <- test_it(lst)
  fails(output, mess_patt = "an error in the first <code>ggvis</code> command")

  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  lst$DC_CODE <- "p <- mtcars %>% ggvis() %>% layer_histograms(~wt)"
  output <- test_it(lst)
  fails(output, mess_patt = "make sure to correctly define the properties")
})

test_that("test props argument", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis'), props = c('x', 'y'))"
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~hp)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~hp) %>% layer_points()"
  output <- test_it(lst)
  fails(output)
})

test_that("test set basic different functions", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'))"
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis() %>% layer_points(~disp, ~mpg)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~hp) %>% layer_points()"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis() %>% layer_points(~disp, ~hp)"
  output <- test_it(lst)
  fails(output)
})

test_that("test set basic allow_extra", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg, size := 80) %>% layer_points()"
  
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
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_CODE <- ""
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The first <code>ggvis</code> command in your code should contain")
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'), not_called_msg = 'silly')"
  output <- test_it(lst)
  fails(output, mess_patt = "Silly")
})

test_that("test missing function calls", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()"
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The first <code>ggvis</code> command in your code should contain")
  
  lst$DC_SCT <- "test_props(index = 1, funs = c('ggvis','layer_points'), not_called_msg = 'silly')"
  output <- test_it(lst)
  fails(output, mess_patt = "Silly")
})

test_that("test with different calls", {
  lst <- list()
  lst$DC_PEC <- "library(ggvis)"
  lst$DC_SOLUTION <- "p <- mtcars %>% ggvis(~disp, ~mpg)\np <- mtcars %>% ggvis(~wt, ~hp)"
  lst$DC_SCT <- "test_props(1, 'ggvis')\ntest_props(2, 'ggvis')"
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg)\np <- mtcars %>% ggvis(~wt, ~hp)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "p <- mtcars %>% ggvis(~disp, ~mpg)\np <- mtcars %>% ggvis(~disp, ~mpg)"
  output <- test_it(lst)
  fails(output)
})
