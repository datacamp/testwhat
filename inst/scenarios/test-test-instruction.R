scen <- list(
  list(
    type = "ChallengeExercise",
    student = "x <- c(1,2,3); y <- 5; m <- mean(x+y); diff(m)",
    solution = "x <- c(1,2,3); y <- 5; m <- mean(x+y); d <- diff(m); d",
    pass = list(
      test_one_instruction = list(
        long = "test passes if one instruction passes",
        sct = "test_instruction(1, test_object(\"x\"))"
      ),
      test_two_instructions = list(
        long = "test passes if two instruction passes",
        sct = "test_instruction(1, test_object(\"x\")); test_instruction(2, test_object(\"y\"))"
      ),
      test_first_fails_second_passes = list(
        long = "test passes if first fails and second passes",
        sct = "test_instruction(1, test_object(\"d\")); test_instruction(2, test_output_contains(\"diff(m)\"))"
      )
    ),
    fail = list(
      test_one_instruction_fails = list(
        long = "test passes if one instruction passes",
        sct = "test_instruction(1, test_object(\"d\"))"
      ),
      test_two_instructions_fail = list(
        long = "test passes if two instruction passes",
        sct = "test_instruction(1, test_object(\"x\")); test_instruction(2, test_object(\"d\"))"
      ),
      test_first_fails_second_fails = list(
        long = "test passes if first fails and second passes",
        sct = "test_instruction(1, output_contains(\"'dansen'\")); test_instruction(2, test_object(\"d\"))"
      )
    )
  )
)