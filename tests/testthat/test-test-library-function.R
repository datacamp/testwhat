context("test_library_function")
source("helpers.R")

test_that("test_library_function works 1", {
  lst <- list()
  lst$DC_CODE <- ""
  
  lst$DC_SCT <- "test_library_function(\"yaml\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_library_function(\"yaml\", not_called_msg = \"blieblabloe\")"
  output <- test_it(lst)
  fails(output, mess_patt = "blieblabloe")
})

test_that("test_library_function works 2", {
  lst <- list()
  lst$DC_SCT <- "test_library_function(\"yaml\")"
  
  lst$DC_CODE <- "library(yaml)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "library(\"yaml\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "library(\'yaml\')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "require(yaml)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "require(\"yaml\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "require(\'yaml\')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "library(ggvis)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "require(\"ggvis\")"
  output <- test_it(lst)
  fails(output)
})
