context("test_function_definition")
source("helpers.R")

# test_that("test_function_definition works", {
#   lst <- list()
#   lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
#   lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}\nmy_punk <- function () { FALSE }"
#   
#   lst$DC_SCT <- "test_function_definition(\"my_func\")"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_result('my_func(3,3)')",
#                       "  test_expression_result('my_func(1,2)')",
#                       "})", sep = "\n")
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_output('my_func(3,3)')",
#                       "  test_expression_output('my_func(1,2)')",
#                       "})", sep = "\n")
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- "test_function_definition(\"my_punk\")"
#   output <- test_it(lst)
#   fails(output, mess_patt = "Did you define")
#   
#   lst$DC_SCT <- "test_function_definition(\"my_punk\", undefined_msg = \"jajajaja\")"
#   output <- test_it(lst)
#   fails(output, mess_patt = "jajajaja")
#   
# })

test_that("test_function_definition works 2", {
  lst <- list()
  lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x - y\n}"
  lst$DC_SOLUTION <- "my_func <- function(x, y, z = 0) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
  
  lst$DC_SCT <- "test_function_definition(\"my_func\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "}, incorrect_number_arguments_msg = \"oeioeioei\")", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "oeioeioei")
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "number of arguments")
  
})

test_that("test_function_definition works 3", {
  lst <- list()
  lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Kalfculating %d plus %d', x, y))\n  x - y\n}"
  lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure that running")
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_output('my_func(3,3)')",
                      "  test_expression_output('my_func(1,2)')",
                      "})", sep = "\n")
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure that .* outputs")
  
})