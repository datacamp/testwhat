context("test_operator")

test_that("test_operator errors correctly", {
  lst <- list()
  lst$DC_SCT <- "test_operator('+')"
  
  lst$DC_SOLUTION <- ""
  output <- test_it(lst)
  error(output, mess_patt = "There aren't 1 \\+ operators available")
  
  lst$DC_SOLUTION <- "test <- function(a) { 4 + a }"
  output <- test_it(lst)
  error(output, mess_patt = "Running 4 \\+ a generated an error")
})

test_that("arithmetic operators and logical operators:", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_SCT <- "test_operator('+')"
  
  lst$DC_CODE <- "7 + 8"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "1 + 3\n7 + 8"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "6 + 9"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "1 + 3\n6 + 9"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mean(7 + 8)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "7 + 6"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "1 + 3\n7 + 6"
  output <- test_it(lst)
  fails(output)
  
  # mathematical operators: + - % * %% ^
  lst <- list()
  lst$DC_CODE <- "1 + 1\n8 - 3\n2 * 2\n3 / 4\n4 ^ 4\n56 %% 43\n"
  lst$DC_SOLUTION <- "1 + 1\n8 - 3\n2 * 2\n3 / 4\n4 ^ 4\n56 %% 43\n"
  lst$DC_SCT <- paste("test_operator('+')", "test_operator('-')", 
                      "test_operator('/')", "test_operator('*')", 
                      "test_operator('^')", "test_operator('%%')", sep = "\n")
  output <- test_it(lst)
  passes(output)
  
  # relational operators:  == != < > <= >= 
  lst <- list()
  lst$DC_CODE <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\n4 == 4\n5 != 7"
  lst$DC_SOLUTION <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\n4 == 4\n5 != 7"
  lst$DC_SCT <- paste("test_operator('<')", "test_operator('>')", 
                      "test_operator('<=')", "test_operator('>=')",
                      "test_operator('==')", "test_operator('!=')", sep = "\n")
  output <- test_it(lst)
  passes(output)
  
  # logical operators: & | && || !
  lst <- list()
  lst$DC_CODE <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\nc(T, T) & c(F, F)\nTRUE && (4 < 3)\nc(T, T) | (4 < 3)\nTRUE || FALSE\n !TRUE"
  lst$DC_SOLUTION <- "1 < 2\n1 > 5\n1 <= 5\nmean(c(1,2,4)) >= 9\nc(T, T) & c(F, F)\nTRUE && (4 < 3)\nc(T, T) | (4 < 3)\nTRUE || FALSE\n !TRUE"
  lst$DC_SCT <- paste("test_operator('&')", "test_operator('|')",
                      "test_operator('&&')", "test_operator('||')", 
                      "test_operator('!')", sep = "\n")
  output <- test_it(lst)
  passes(output)
})

test_that("test_operator messaging", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  
  lst$DC_CODE <- ""
  lst$DC_SCT <- "test_operator('+')"
  output <- test_it(lst)
  fails(output, mess_patt = "The system wants to check the first")
  
  lst$DC_SCT <- "test_operator('+', not_called_msg = 'test')"
  output <- test_it(lst)
  fails(output, mess_patt = "test")
  
  lst$DC_CODE <- "4 + 5"
  lst$DC_SCT <- "test_operator('+')"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you correctly used")
  line_info(output, 1, 1)
  
  lst$DC_SCT <- "test_operator('+', incorrect_msg = 'wrong')"
  output <- test_it(lst)
  fails(output, mess_patt = "wrong")
  line_info(output, 1, 1)
})

test_that("test_operator with eval", {
  lst <- list()
  lst$DC_SOLUTION <- "7 + 8"
  lst$DC_CODE <- "4 + 5"
  
  lst$DC_SCT <- "test_operator('+')"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_operator('+', eval = FALSE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_operator with indexes", {
  lst <- list()
  lst$DC_SOLUTION <- "4 + 5\n10 + 20"
  lst$DC_SCT <- paste("test_operator('+', index = 1, not_called_msg = 'ncm1', incorrect_msg = 'icm1')",
                      "test_operator('+', index = 2, not_called_msg = 'ncm2', incorrect_msg = 'icm2')", sep = "\n")
  
  lst$DC_CODE <- "4 + 5\n10 + 20"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "10 + 20\n4 + 5"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = 'ncm1')
  
  lst$DC_CODE <- "4 + 5\n4 + 5"
  output <- test_it(lst)
  fails(output, mess_patt = 'icm2')
  line_info(output, 2, 2)
  
  lst$DC_CODE <- "4 + 5"
  output <- test_it(lst)
  fails(output, mess_patt = 'ncm2')
  
  lst$DC_CODE <- "10 + 20"
  output <- test_it(lst)
  fails(output, mess_patt = 'icm1')
  line_info(output, 1, 1)
})
