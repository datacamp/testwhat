context("test_object")
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