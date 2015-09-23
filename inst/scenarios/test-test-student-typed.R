scen <- list(
  list(
    type = "NormalExercise",
    student = "print('some crazy output')",
    pass = list(
      test_student_typed_pass = list(
        long = "test succeeds if student typed the string",
        sct = "test_student_typed(\"print('some crazy output')\")"
      ),
      test_student_typed_pass_2_options = list(
        long = "test succeeds if student typed one of two strings",
        sct = "test_student_typed(c(\"print('some crazy output')\", \"not this\"))"
      ),
      test_student_typed_pattern = list(
        long = "test succeeds if student typed the pattern",
        sct = "test_student_typed(\"print.*\", fixed = FALSE)"
      )
    ),
    fail = list(
      test_student_typed_fail = list(
        long = "test fails if student didn't type the string",
        sct = "test_student_typed(\"print('some wrong output')\", not_typed_msg = \"You didn't type it\")",
        message = "You didn't type it"
      ),
      test_student_typed_fail_2_options = list(
        long = "test fails if student didn't type the string",
        sct = "test_student_typed(c(\"print('some wrong output')\", \"not this\"), not_typed_msg = \"You didn't type anything\")",
        message = "You didn't type anything"
      ),
      test_student_typed_pattern_fails = list(
        long = "test succeeds if student didn't type the pattern",
        sct = "test_student_typed(\"pront.*\", fixed = FALSE, not_typed_msg = \"not pronting\")",
        message = "not pronting"
      )
    )
  )
)