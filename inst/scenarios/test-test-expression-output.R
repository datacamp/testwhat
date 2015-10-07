# Tested more extensively in test-test-function-definition.R

scen <- list(
  list(
    type = "NormalExercise",
    student = "func <- function(x) { print(x) }\nfunc_not_eq <- function(x) { print(paste(\"Output:\",x)) }",
    solution = "func <- function(x) { print(x) }\nfunc_not_eq <- function(x) { print(x) }",
    pass = list(
      test_output_equal = list(
        long = "test succeeds if output of expr are equal",
        sct = "test_expression_output(\"func(3)\")"
      )
    ),
    fail = list(
      test_output_equal = list(
        long = "test fails if output of expr are not equal",
        sct = "test_expression_output(\"func_not_eq(3)\")",
        message = "Make sure that.*outputs.*"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "func <- function(x) { x }",
    solution = "func <- function(x) { x - 1 }",
    fail = list(
      test_output_not_equal_2 = list(
        long = "test fails if outputs of expr are not equal",
        sct = "test_expression_output(\"func(3)\")"
      )
    )
  )
)