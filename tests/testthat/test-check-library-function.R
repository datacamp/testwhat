context("test_library_function")

test_that("test/check_library(_function) works", {
  for (sct in c("test_library_function('yaml')",
                "ex() %>% check_library('yaml')")) {
    lst <- list(DC_SCT = sct)
    for (s in c("library(yaml)",
                "library('yaml')",
                "library(\"yaml\")",
                "require(yaml)",
                "require('yaml')",
                "require(\"yaml\")")) {
      lst$DC_CODE <- s
      output <- test_it(lst)
      passes(output)  
    }
    
    for (s in c("", "library(ggvis)", "require('ggvis')")) {
      lst$DC_CODE <- s
      output <- test_it(lst)
      fails(output)
    }
  }
})

test_that("test/check_library(_function) messaging", {
  for (sct in c("test_library_function('yaml', not_called_msg = 'NCM', incorrect_msg = 'ICM')",
                "ex() %>% check_library('yaml', not_called_msg = 'NCM', incorrect_msg = 'ICM')")) {
    lst <- list(DC_SCT = sct)    
    lst$DC_CODE <- ""
    output <- test_it(lst)
    fails(output, "NCM")
    
    lst$DC_CODE <- "#library("
    output <- test_it(lst)
    fails(output, "NCM")
    
    lst$DC_CODE <- "library('ggvis')"
    output <- test_it(lst)
    fails(output, "ICM")
  }
})

