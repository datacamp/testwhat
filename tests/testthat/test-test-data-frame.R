context("test_data_frame")

test_that("test_df - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_SCT <- "dfstate <- ex() %>% test_obj('df')
                 dfstate %>% test_col('a') %>% test_equal()
                 dfstate %>% test_col('b') %>% test_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define the variable")
  
  lst$DC_CODE <- "df <- data.frame(c = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>a</code>")
  
  lst$DC_CODE <- "df <- data.frame(a = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>a</code> doesn&#39;t seem to be correct")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>b</code>")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('r', 's', 't'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>b</code> doesn&#39;t seem to be correct")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_df - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_SCT <- "dfstate <- ex() %>% test_obj('df', undefined_msg = 'undefined')
  dfstate %>% test_col('a', col_missing_msg = 'missinga') %>% test_equal(incorrect_msg = 'incorra')
  dfstate %>% test_col('b', col_missing_msg = 'missingb') %>% test_equal(incorrect_msg = 'incorrb')"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Undefined")
  
  lst$DC_CODE <- "df <- data.frame(c = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Missinga")
  
  lst$DC_CODE <- "df <- data.frame(a = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Incorra")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Missingb")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('r', 's', 't'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Incorrb")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_df - backwards compatibility", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_SCT <- "test_data_frame('df')"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define the variable")
  
  lst$DC_CODE <- "df <- data.frame(c = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>a</code>")
  
  lst$DC_CODE <- "df <- data.frame(a = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>a</code> doesn&#39;t seem to be correct")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>b</code>")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('r', 's', 't'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>b</code> doesn&#39;t seem to be correct")
  
  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_df - eq_condition", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_CODE <- "df <- data.frame(a = c(1 + 4.4e-9, 2, 3), b = c('x', 'y', 'z'), row.names = c('r', 's', 't'))"

  lst$DC_SCT <- "ex() %>% test_obj('df') %>% test_col('a') %>% test_equal(eq_condition = 'equivalent')"
  output <- test_it(lst)
  passes(output)
  
  # Maybe the different row names should cause a fail here... ?
  lst$DC_SCT <- "ex() %>% test_obj('df') %>% test_col('a') %>% test_equal(eq_condition = 'equal')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "ex() %>% test_obj('df') %>% test_col('a') %>% test_equal(eq_condition = 'identical')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_df - line numbers", {
  # TODO
})


