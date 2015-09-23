scen <- list(
  list(
    type = "NormalExercise",
    student = "a = 3
    if (a == 3) { print('equal') } else { print('not equal') }",
    solution = "a = 3
    if (a == 3) { print('equal') } else { print('not equal') }",
    pass = list(
      test_passes_cond = list(
        long = "test succeeds with right condition test",
        sct = "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") })"
      ),
      test_passes_expr = list(
        long = "test succeeds with right expression test",
        sct = "test_if_else(if_expr_test = { test_function(\"print\", \"x\") })"
      ),
      test_passes_else = list(
        long = "test succeeds with right else expression test",
        sct = "test_if_else(else_expr_test = { test_function(\"print\", \"x\") })"
      ),
      test_passes_all = list(
        long = "test succeeds with all the right tests",
        sct = "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
                            if_expr_test = { test_function(\"print\", \"x\") },
                            else_expr_test = { test_function(\"print\", \"x\") })"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "a = 4
    if (a == 4) { print('not equal') } else { print('equal') }",
    solution = "a = 3
    if (a == 3) { print('equal') } else { print('not equal') }",
    fail = list(
      test_fails_cond = list(
        long = "test fails with right condition test",
        sct = "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") })"
      ),
      test_fails_expr = list(
        long = "test fails with right expression test",
        sct = "test_if_else(if_expr_test = { test_function(\"print\", \"x\") })"
      ),
      test_fails_else = list(
        long = "test fails with right else expression test",
        sct = "test_if_else(else_expr_test = { test_function(\"print\", \"x\") })"
      ),
      test_fails_all = list(
        long = "test fails with all the right tests",
        sct = "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
                            if_expr_test = { test_function(\"print\", \"x\") },
                            else_expr_test = { test_function(\"print\", \"x\") })"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "a = 4
    if (a == 4) { print('not equal') }",
    solution = "a = 3
    if (a == 3) { print('equal') } else { print('not equal') }
    if (3 == 3) { invisible() }",
    fail = list(
      test_missing_else = list(
        long = "test fails with missing else",
        sct = "test_if_else(else_expr_test = { test_function(\"print\", \"x\") }, missing_else_msg = \"NO ELSE\" )",
        message = "NO ELSE"
      ),
      test_not_found = list(
        long = "test fails with missing if",
        sct = "test_if_else(2,
                            if_cond_test = { test_student_typed(\"a == 3\") },
                            if_expr_test = { test_function(\"print\", \"x\") },
                            else_expr_test = { test_function(\"print\", \"x\")},
                            not_found_msg = \"NO IF\")",
        message = "NO IF"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "a = 3
    if (3 == 1) { print('visible') }
    if (a == 3) { print('equal') } else { print('not equal') }",
    solution = "a = 3
    if (3 == 3) { invisible() }
    if (a == 3) { print('equal') } else { print('not equal') }",
    pass = list(
      test_pass_with_index = list(
        long = "test succeeds with index",
        sct = "test_if_else(2,
                            if_cond_test = { test_student_typed(\"a == 3\") },
                            if_expr_test = { test_function(\"print\", \"x\") },
                            else_expr_test = { test_function(\"print\", \"x\")},
                            not_found_msg = \"NO IF\")"
      )
    ),
    fail = list(
      test_missing_else_with_index = list(
        long = "test fails with missing else with index",
        sct = "test_if_else(1, else_expr_test = { test_function(\"print\", \"x\") }, missing_else_msg = \"NO ELSE\" )",
        message = "NO ELSE"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "a = 3
    if (a == 3) { print('equal') } else if (a == 4) { print('not equal') } else { print('zever, geen gezever') }",
    solution = "a = 3
    if (a == 3) { print('equal') } else if (a == 4) { print('not equal') } else { print('zever, gezever') }",
    pass = list(
      test_pass_with_nested = list(
        long = "test succeeds with nested ifs",
        sct = "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
                            if_expr_test = { test_function(\"print\", \"x\") },
                            else_expr_test = { 
                                test_if_else(if_cond_test = { test_student_typed(\"a == 4\") },
                                             if_expr_test = { test_function(\"print\", \"x\") },
                                             else_expr_test = { test_function(\"print\")})})"
      )
    ),
    fail = list(
      test_fails_with_nested = list(
        long = "test succeeds with nested ifs",
        sct = "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
                            if_expr_test = { test_function(\"print\", \"x\") },
                            else_expr_test = { 
                                test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
                                             if_expr_test = { test_function(\"print\", \"x\") },
                                             else_expr_test = { test_function(\"print\", \"x\")})})"
      )
    )
  )
)