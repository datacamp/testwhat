context("test_file_exists")
source("helpers.R")

test_that("test_file_exists works", {
  lst <- list()
  lst$DC_PEC <- "if (file.exists(\"testing.txt\")) file.remove(\"testing.txt\")"
  lst$DC_CODE <- "\n  write(\"testing\", file = \"testing.txt\")"
  lst$DC_SCT <- "test_file_exists(\"testing.txt\")\nfile.remove(\"testing.txt\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_file_exists(\"this_filename_can_not_possibly_exist.txt\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_file_exists(\"this_filename_can_not_possibly_exist.txt\", incorrect_msg = \"You crazy moth...\")"
  output <- test_it(lst)
  fails(output, mess_patt = "You crazy")
  
})