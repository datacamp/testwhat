context("utils")

test_that("backwards comp", {
  e <- new.env(globalenv())
  tw$set(state = RootState$new(solution_env = e,
                               student_code = "abc",
                               solution_code = "abc"))
  expect_equal(get_solution_env(), e)
  expect_equal(get_student_code(), "abc")
  expect_equal(get_solution_code(), "abc")
})


test_that("accessor works", {
  tw <- tw_accessors()
  tw$initialize(data = list(a = 2, b = 3, c = 4))
  expect_equal(tw$get("a"), 2)
  expect_equal(tw$get("b"), 3)
  expect_equal(tw$get("c"), 4)
  expect_equal(tw$get("d"), NULL)
  tw$set(d = 5)
  expect_equal(tw$get("a"), 2)
  expect_equal(tw$get("b"), 3)
  expect_equal(tw$get("c"), 4)
  expect_equal(tw$get("d"), 5)
  tw$clear()
  expect_equal(tw$get(), list())
  expect_equal(tw$get("a"), NULL)
})


test_that("check_output - basic", {
  # Student envrionment manipulation
  lst <- list()
  lst$DC_ECHO <- TRUE
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "tryCatch(test_object('x'), finally = execute_student(rm(x)))"
  
  # correct answer
  lst$DC_CODE <- "x <- 5"
  output <- test_it(lst)
  passes(output)
  # should be cleaned up
  expect_equal(length(ls(envir = .GlobalEnv)), 0)
  expect_equal(length(ls(envir = RBackend:::dc$get("sol_env"))), 1)
  
  # incorrect answer
  lst$DC_CODE <- "x <- 7"
  output <- test_it(lst)
  fails(output)
  # should also be cleaned up
  expect_equal(length(ls(envir = .GlobalEnv)), 0)
  expect_equal(length(ls(envir = RBackend:::dc$get("sol_env"))), 1)
  
  # Solution envrionment manipulation
  lst <- list()
  lst$DC_ECHO <- TRUE
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "tryCatch(test_object('x'), finally = execute_solution(rm(x)))"
  
  # correct answer
  lst$DC_CODE <- "x <- 5"
  output <- test_it(lst)
  passes(output)
  # should be cleaned up
  expect_equal(length(ls(envir = .GlobalEnv)), 1)
  expect_equal(length(ls(envir = RBackend:::dc$get("sol_env"))), 0)
  
  # incorrect answer
  lst$DC_CODE <- "x <- 7"
  output <- test_it(lst)
  fails(output)
  # should also be cleaned up
  expect_equal(length(ls(envir = .GlobalEnv)), 1)
  expect_equal(length(ls(envir = RBackend:::dc$get("sol_env"))), 0)
})
