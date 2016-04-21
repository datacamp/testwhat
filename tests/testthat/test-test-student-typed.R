context("test_student_typed")
source("helpers.R")

test_that("test_student_typed works", {
  lst <- list()
  lst$DC_CODE <- "print('some crazy output')"
  
  lst$DC_SCT <- "test_student_typed(\"print('some crazy output')\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_student_typed(c(\"print('some crazy output')\", \"not this\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_student_typed(\"print.*\", fixed = FALSE)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_student_typed(\"print('some wrong output')\", not_typed_msg = \"You didnt type it\")"
  output <- test_it(lst)
  fails(output, mess_patt = "You didnt type it")
  
  lst$DC_SCT <- "test_student_typed(c(\"print('some wrong output')\", \"not this\"), not_typed_msg = \"You didnt type anything\")"
  output <- test_it(lst)
  fails(output, mess_patt = "You didnt type anything")
  
  lst$DC_SCT <- "test_student_typed(\"pront.*\", fixed = FALSE, not_typed_msg = \"not pronting\")"
  output <- test_it(lst)
  fails(output, mess_patt = "not pronting")

})

test_that("test_student_typed works with times argument", {
  lst <- list()
  lst$DC_SCT <- "test_student_typed(c('a', 'b'), times = 2)"
  
  lst$DC_CODE <- "ab"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "aa"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "bb"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "aba"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "abb"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "bbb"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "a"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "b"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "c"
  output <- test_it(lst)
  fails(output)
})