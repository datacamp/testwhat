scen <- list(
  list(
    type = "MultipleChoiceExercise",
    choice = 1,
    pass = list(
      test_right_choice = list(
        long = "test succeeds if choice is correct",
        sct = "test_mc(1, feedback_msgs = c('this is the CORRECT answer', 'this is the WRONG answer'))"
      )
    ),
    fail = list(
      test_right_choice = list(
        long = "test fails if choice is incorrect",
        sct = "test_mc(2, feedback_msgs = c('this is the WRONG answer', 'this is the CORRECT answer'))",
        message = "this is the WRONG answer"
      )
    )
  )
)