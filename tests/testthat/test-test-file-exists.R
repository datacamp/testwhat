context("test_file_exists")

## Really funky stuff with these tests...

test_that("check_wd - 1", {
  lst <- list()
  lst$DC_PEC <- "if (file.exists('testing.txt')) file.remove('testing.txt')"
  lst$DC_CODE <- "write('testing', file = 'testing.txt')"

  lst$DC_SCT <- "ex() %>% check_wd(path = 'testing.txt'); suppressWarnings(file.remove('testing.txt'))"
  output <- test_it(lst)
  passes(output)
})

test_that("check_wd - 2", {
  lst <- list() # no pec, no solution, no user code

  lst$DC_SCT <- "ex() %>% check_wd('non_existing.txt')"
  output <- test_it(lst)
  fails(output, mess_patt = "The file <code>non_existing.txt</code> does not appear to be in your working directory")

  lst$DC_SCT <- "ex() %>% check_wd('non_existing.txt', missing_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})

test_that("check_wd - 3", {
  lst <- list() # no pec, no solution, no user code

  lst$DC_SCT <- "ex() %>% check_wd('test/non_existing.txt')"
  output <- test_it(lst)
  fails(output, mess_patt = "The file <code>non_existing.txt</code> does not appear to be inside the folder <code>test</code> in your working directory")

  lst$DC_SCT <- "ex() %>% check_wd('test/non_existing.txt', missing_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})

test_that("test_file_exists - backwards compatible - 1", {
  lst <- list()
  lst$DC_PEC <- "if (file.exists('testing.txt')) file.remove('testing.txt')"
  lst$DC_CODE <- "write('testing', file = 'testing.txt')"

  lst$DC_SCT <- "test_file_exists('testing.txt'); file.remove('testing.txt')"
  output <- test_it(lst)
  passes(output)
})

test_that("test_file_exists - backwards compatible - 2", {
  lst <- list() # no pec, no solution, no user code

  lst$DC_SCT <- "test_file_exists('non_existing.txt')"
  output <- test_it(lst)
  fails(output, mess_patt = "The file <code>non_existing.txt</code> does not appear to be in your working directory")

  lst$DC_SCT <- "test_file_exists('non_existing.txt', incorrect_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})

test_that("test_file_exists - backwards compatible - 3", {
  lst <- list() # no pec, no solution, no user code

  lst$DC_SCT <- "test_file_exists('test/non_existing.txt')"
  output <- test_it(lst)
  fails(output, mess_patt = "The file <code>non_existing.txt</code> does not appear to be inside the folder <code>test</code> in your working directory")

  lst$DC_SCT <- "test_file_exists('test/non_existing.txt', incorrect_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})