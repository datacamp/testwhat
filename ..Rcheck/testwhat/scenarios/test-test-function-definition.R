scen <- list(
  list(
    type = "NormalExercise",
    student = "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}",
    solution = "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}\nmy_punk <- function () { FALSE }",
    pass = list(
      test_simpe_pass = list(
        long = "test succeeds if the function is defined",
        sct = "test_function_definition(\"my_func\")"
      ),
      test_func_same_result = list(
        long = "test succeeds if the function result is the same",
        sct = c(
          "test_function_definition(\"my_func\", function_test = {",
          "  test_expression_result('my_func(3,3)')",
          "  test_expression_result('my_func(1,2)')",
          "})"
        )
      ),
      test_func_same_output = list(
        long = "test succeeds if the function output is the same",
        sct = c(
          "test_function_definition(\"my_func\", function_test = {",
          "  test_expression_output('my_func(3,3)')",
          "  test_expression_output('my_func(1,2)')",
          "})"
        )
      )
    ),
    fails = list(
      test_simpe_fail = list(
        long = "test fails if the function is undefined",
        sct = "test_function_definition(\"my_punk\")",
        message = "Did you define"
      ),
      test_simpe_fail = list(
        long = "test fails if the function is undefined",
        sct = "test_function_definition(\"my_punk\", undefined_msg = \"jajajaja\")",
        message = "jajajaja"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x - y\n}",
    solution = "my_func <- function(x, y, z = 0) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}",
    pass = list(
      test_passes_without_function = list(
        long = "test succeeds with no function test and wrong number of arguments",
        sct = "test_function_definition(\"my_func\")"
      )
    ),
    fail = list(
      test_func_correct_feedback = list(
        long = "test fails with correct feedback if result is not the same and not same n of arguments",
        sct = c(
          "test_function_definition(\"my_func\", function_test = {",
          "  test_expression_result('my_func(3,3)')",
          "  test_expression_result('my_func(1,2)')",
          "}, incorrect_number_arguments_msg = \"oeioeioei\")"
        ),
        message = "oeioeioei"
      ),
      test_func_correct_feedback_2 = list(
        long = "test fails with correct feedback if result is not the same and not same n of arguments",
        sct = c(
          "test_function_definition(\"my_func\", function_test = {",
          "  test_expression_result('my_func(3,3)')",
          "  test_expression_result('my_func(1,2)')",
          "})"
        ),
        message = "number of arguments"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "my_func <- function(x, y) {\n  print(sprintf('Kalfculating %d plus %d', x, y))\n  x - y\n}",
    solution = "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}",
    fail = list(
      test_func_correct_feedback = list(
        long = "test fails with correct feedback if result is not the same and not same n of arguments",
        sct = c(
          "test_function_definition(\"my_func\", function_test = {",
          "  test_expression_result('my_func(3,3)')",
          "  test_expression_result('my_func(1,2)')",
          "})"
        ),
        message = "Make sure that running"
      ),
      test_func_correct_feedback_2 = list(
        long = "test fails with correct feedback if result is not the same and not same n of arguments",
        sct = c(
          "test_function_definition(\"my_func\", function_test = {",
          "  test_expression_output('my_func(3,3)')",
          "  test_expression_output('my_func(1,2)')",
          "})"
        ),
        message = "Make sure that .* outputs"
      )
    )
  )
)