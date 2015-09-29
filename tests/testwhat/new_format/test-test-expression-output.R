scen <- list(
  list(
    type = "NormalExercise",
    student = "func <- function(x) { print(x) }",
    solution = "func <- function(x) { print(x) }",
    pass = list(
      test_result_equal = list(
        long = "test succeeds if result of expr are equal",
        sct = "test_expression_output(\"func('This is a test')\")"
      )
    )
  )
)