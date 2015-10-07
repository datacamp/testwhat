# Tested more extensively in test-test-function-definition.R

scen <- list(
  list(
    type = "NormalExercise",
    student = "func <- function(x) { x + 3 }\nfunc_not_eq <- function(x) { x + 4 }",
    solution = "func <- function(x) { x + 3 }\nfunc_not_eq <- function(x) { x + 3 }",
    pass = list(
      test_result_equal = list(
        long = "test succeeds if result of expr are equal",
        sct = "test_expression_result(\"func(3)\")"
      )
    ),
    fail = list(
      test_result_equal = list(
        long = "test fails if result of expr are not equal",
        sct = "test_expression_result(\"func_not_eq(3)\")",
        message = "Make sure that.*results in.*"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "func <- function(x) { x / 3 }",
    solution = "func <- function(x) { x + 3 }",
    fail = list(
      test_result_not_equal = list(
        long = "test fails if result of expr are not equal",
        sct = "test_expression_result(\"func(3)\")"
      )
    )
  )
)