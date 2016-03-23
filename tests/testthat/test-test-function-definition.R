context("test_function_definition")
source("helpers.R")

test_that("test_function_definition works", {
  lst <- list()
  lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
  lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}\nmy_punk <- function () { FALSE }"
  
  lst$DC_SCT <- "test_function_definition(\"my_func\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_output('my_func(3,3)')",
                      "  test_expression_output('my_func(1,2)')",
                      "})", sep = "\n")
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function_definition(\"my_punk\")"
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define")
  
  lst$DC_SCT <- "test_function_definition(\"my_punk\", undefined_msg = \"jajajaja\")"
  output <- test_it(lst)
  fails(output, mess_patt = "jajajaja")
  
})

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

test_that("test_function_definition works with control structure in there", {
  lst <- list()
  lst$DC_SOLUTION <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    print(NULL)\n  }}"
  lst$DC_SCT <- paste("test_function_definition('my_filter',", 
                      "function_test = {\ntest_expression_result('my_filter(5)')\ntest_expression_output('my_filter(-5)')\n},",
                      "body_test = test_if_else(index = 1, if_cond_test = test_student_typed(c(\">= 0\",\"0 =<\")), else_expr_test = test_function('print', 'x')))", sep = "")
  
  lst$DC_CODE <- "my_filter <- function (x) {\n  if (x > 0) {\n    print(x)\n  } else {\n    print(\"NULL\")\n  }}"
  output <- test_it(lst)
  fails(output, mess_patt = "appropriate location")
  
  lst$DC_CODE <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    str(\"NULL\")\n  }}"
  output <- test_it(lst)
  fails(output, mess_patt = "system wants to check")
  
  lst$DC_CODE <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    print(\"NULL\")\n  }}"
  output <- test_it(lst)
  fails(output, mess_patt = "argument <code>x</code>")
  line_info(output, 5, 5)
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})