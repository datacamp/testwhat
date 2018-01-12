context("check_file")

file_state <- function(STU_CODE, SOL_CODE, PEC = character()) {
  tw <<- testwhat:::tw
  tw$clear()
  
  state <- testwhat:::FileSysState$new(
    pec = PEC,
    student_code = STU_CODE,
    student_pd = NULL,
    #student_env = stu_env,
    solution_code = SOL_CODE,
    solution_pd = NULL,
    #solution_env = sol_env,
    output_list = list(),
    test_env = new.env(parent=globalenv())
  )
  
  # testwhat will access the reporter and state from the tw object
  rep <- testwhat:::DC_reporter$new()
  tw$set(state = state, reporter = rep, stack = TRUE)
  tw$get("state")
}

CODE_SIMPLE <- list(
  STU_CODE = list('right.R' = "'right'", 'wrong.R' = '""'),
  SOL_CODE = list('right.R' = "'right", 'wrong.R' = '"wrong"')
)

test_that("check_file finds file", {
  state <- do.call(file_state, CODE_SIMPLE)
  
  fstate <- check_file(state, 'wrong.R')
  
  expect_equal(fstate$get("student_code"), '""')
  expect_equal(fstate$get("solution_code"), '"wrong"')
})

test_that("check_file raises error when file missing from solution", {
  state <- do.call(file_state, CODE_SIMPLE)
  
  expect_error(testwhat::check_file(state, 'zeebo.R'), 
               "file name zeebo.R not in right.R, wrong.R")
})

test_that("check_file fails test when file missing from submission", {
  state <- file_state(STU_CODE = list('script.R' = '1 + 1'),
                      SOL_CODE = list('script.R' = '1 + 1', 'other.R' = '2 + 2'))
  
  expect_error(check_file(state, 'other.R'), "<sct_failed_error>")
})

test_that("check_file followed by test_student_typed correct", {
  state <- do.call(file_state, CODE_SIMPLE)
  
  fstate <- check_file(state, 'right.R')
  expect_silent(
    check_code(fstate, "right", fixed = TRUE)
  )
})

test_that("check_file followed by test_student_typed incorrect", {
  state <- do.call(file_state, CODE_SIMPLE)

  fstate <- check_file(state, 'wrong.R')
  
  expect_error(check_code(fstate, "wrong", fixed = TRUE), "<sct_failed_error>")
})

test_that("check_file parses new file correctly", {
  state <- do.call(file_state, CODE_SIMPLE)
  fstate <- check_file(state, 'right.R')
  expect_equivalent(fstate$get('student_pd'), testwhat:::build_pd("'right'"))
})

test_that("check_file misparse uses null for parse data", {
  state <- file_state(STU_CODE = list('script.R' = '_1'), SOL_CODE = list('script.R' = '1'))
  fstate <- check_file(state, 'script.R')
  expect_null(fstate$get("student_pd"))
})

# test test_exercise with check_file ----

pf_test_ex <- function(sct) test_exercise(
    sct = sct,
    ex_type = "NormalExercise",
    pec = "",
    student_code = list('right.R' = "'right'", 'wrong.R' = '""'),
    solution_code = list('right.R' = "'right", 'wrong.R' = '"wrong"'),
    solution_env = new.env(),
    output_list = list()
)

test_that("test_exercise works with check_file", {
  output <- pf_test_ex("ex() %>% check_file('right.R') %>% check_code('right', fixed = TRUE)")

  expect_true(output$correct)
  
})

test_that("test_exercise works with check_file", {
  output <- pf_test_ex("ex() %>% check_file('wrong.R') %>% check_code('wrong', fixed = TRUE)")

  expect_false(output$correct)
})