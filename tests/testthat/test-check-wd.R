context("check_wd")

test_that("check_wd - 1", {
  withr::with_file("testing.txt", {
    writeLines("test", con = "testing.txt")
    s <- setup_state(stu_code = "")
    passes2(ex() %>% check_wd(path = 'testing.txt'))
    passes2(test_file_exists('testing.txt'))
  })
})

test_that("check_wd - 2", {
  expect_error(ex() %>% check_wd('non_existing.txt'),
               regexp = "The file `non_existing.txt` does not appear to be",
               class = "sct_failure")
  expect_error(test_file_exists('non_existing.txt'),
               regexp = "The file `non_existing.txt` does not appear to be",
               class = "sct_failure")

  expect_error(ex() %>% check_wd('non_existing.txt', missing_msg = 'incorrect'),
               regexp = "Incorrect",
               class = "sct_failure")
  expect_error(test_file_exists('non_existing.txt', incorrect_msg = 'incorrect'),
               regexp = "Incorrect",
               class = "sct_failure")
})

test_that("check_wd - 3", {
  expect_error(ex() %>% check_wd('test/non_existing.txt'),
               regexp = "The file `non_existing.txt` does not appear to be inside the folder `test` in your working directory",
               class = "sct_failure")
  expect_error(test_file_exists('test/non_existing.txt'),
               regexp = "The file `non_existing.txt` does not appear to be inside the folder `test` in your working directory",
               class = "sct_failure")

  expect_error(ex() %>% check_wd('test/non_existing.txt', missing_msg = "incorrect"),
               regexp = "Incorrect",
               class = "sct_failure")
  expect_error(test_file_exists('test/non_existing.txt', incorrect_msg = "incorrect"),
               regexp = "Incorrect",
               class = "sct_failure")

})