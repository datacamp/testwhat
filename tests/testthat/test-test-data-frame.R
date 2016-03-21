context("test_for_loop")
source("helpers.R")

test_that("test_data_frame works", {
  lst <- list()
  lst$DC_CODE <- "df.equiv <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equiv <- data.frame(a = c(1, 4, 3), b = c(\"a\", \"b\", \"c\"))"
  lst$DC_SOLUTION <- "df.equiv <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"), c = c(1, 1, 1))\n  df.not_here <- data.frame(a = c(7, 8, 9), b = c(\"a\", \"b\", \"c\"\n))"
  lst$DC_SCT <- "test_data_frame(\"df.equiv\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.not_equiv\", columns = \"b\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.not_equiv\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.not_equiv\", columns = \"a\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.not_here\", undefined_msg = \"data frame not here\")"
  output <- test_it(lst)
  fails(output, mess_patt = "data frame not here")
  
  lst$DC_SCT <- "test_data_frame(\"df.not_equiv\", columns = c(\"c\", \"b\"), undefined_cols_msg = \"data frame column not here\")"
  output <- test_it(lst)
  fails(output, mess_patt = "data frame column not here")
  
  lst$DC_SCT <- "test_data_frame(\"df.not_equiv\", columns = c(\"a\", \"b\"), incorrect_msg = \"data frame column not correct\")"
  output <- test_it(lst)
  fails(output, mess_patt = "data frame column not correct")
})

test_that("test_data_frame works with eq_condition", {
  lst <- list()
  lst$DC_CODE <- "df.equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"\n))\n  rownames(df.not_equal) <- c(\"one\", \"two\", \"three\")"
  lst$DC_SOLUTION <- "df.equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"\n))\n  rownames(df.not_equal) <- c(\"one\", \"oops\", \"three\")"

  lst$DC_SCT <- "test_data_frame(\"df.not_equal\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.equal\", eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.not_equal\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
})

test_that("test_data_frame works with eq_condition", {
  lst <- list()
  lst$DC_CODE <- "df.iden <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_iden <- data.frame(a = c(1 + 4.4e-9, 2, 3), b = c(\"a\", \"b\", \"c\"\n))"
  lst$DC_SOLUTION <- "df.iden <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_iden <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"\n))"
  
  lst$DC_SCT <- "test_data_frame(\"df.not_iden\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.iden\", eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_data_frame(\"df.not_iden\", eq_condition = \"identical\")"
  output <- test_it(lst)
  fails(output)
})