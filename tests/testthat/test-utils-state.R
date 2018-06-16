context("utils-state")

test_that("setup_state works", {
  code <- "x <- 5\nx"
  s <- setup_state(stu_code = code, sol_code = code, pec = "y <- 6")
  expect_equal(get("x", s$get("student_env")), 5)
  expect_equal(get("x", s$get("solution_env")), 5)
  expect_equal(get("y", s$get("student_env")), 6)
  expect_equal(get("y", s$get("solution_env")), 6)
  expect_equal("[1] 5", s$get("output_list")[[length(s$get("output_list"))]]$payload)
  
  s <- setup_state(stu_code = code)
  expect_equal(get("x", s$get("student_env")), 5)
  expect_false("y" %in% ls(s$get("student_env")))
  expect_false("x" %in% ls(s$get("solution_env")))
  expect_false("y" %in% ls(s$get("solution_env")))
  expect_equal("[1] 5", s$get("output_list")[[length(s$get("output_list"))]]$payload)
  
  s <- setup_state(sol_code = code)
  expect_equal(get("x", s$get("solution_env")), 5)
  expect_false("y" %in% ls(s$get("solution_env")))
  expect_false("x" %in% ls(s$get("student_env")))
  expect_false("y" %in% ls(s$get("student_env")))
  
  s <- setup_state()
  expect_equal(length(s$get("solution_env")), 0)
  expect_equal(length(s$get("student_env")), 0)
  
  erroneous_code <- "x"
  expect_error(setup_state(sol_code = erroneous_code))
  s <- setup_state(stu_code = erroneous_code, sol_code = code)
  expect_true("r-error" %in% sapply(s$get("output_list"), `[[`, "type"))
  expect_equal("Error: object 'x' not found", s$get("output_list")[[length(s$get("output_list"))]]$payload)
})
