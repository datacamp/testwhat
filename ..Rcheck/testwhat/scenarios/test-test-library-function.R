scen <- list(
  list(
    type = "NormalExercise",
    pre_exercise_code = "library(yaml)",
    student = "",
    solution = "yaml.load(\"test: test\")",
    fail = list(
      test_not_called = list(
        long = "test fails when no library function is called",
        sct = "test_library_function(\"yaml\")"
      ),
      test_not_called_with_msg = list(
        long = "test fails when no library function is called with not_called_msg",
        sct = "test_library_function(\"yaml\", not_called_msg = \"blieblabloe\")",
        message = "blieblabloe"
      )
    )
  ),
  list(
    type = "NormalExercise",
    pre_exercise_code = "library(yaml)",
    student = "library(yaml)",
    solution = "yaml.load(\"test: test\")",
    pass = list(
      test_correct_library = list(
        long = "test succeeds when the correct library function is called",
        sct = "test_library_function(\"yaml\")"
      )
    ),
    fail = list(
      test_incorrect_library = list(
        long = "test fails when the incorrect library function is called",
        sct = "test_library_function(\"ggvis\")"
      ),
      test_incorrect_library_with_msg = list(
        long = "test fails when the incorrect library function is called with incorrect msg",
        sct = "test_library_function(\"ggvis\", incorrect_msg = \"bloeblablie\")",
        message = "bloeblablie"
      )
    )
  ),
  list(
    type = "ChallengeExercise",
    pre_exercise_code = "library(yaml)",
    student = "library(yaml)",
    solution = "yaml.load(\"test: test\")",
    pass = list(
      test_correct_eventually_chall = list(
        long = "test succeeds when the correct library function is called eventually in challenge",
        sct = "test_instruction(1, test_library_function(\"ggvis\")); test_instruction(2, test_library_function(\"yaml\"))"
      )
    ),
    fail = list(
      test_not_called_chall = list(
        long = "test fails when no library function is called in challenge",
        sct = "test_instruction(1,test_library_function(\"ggvis\"))"
      ),
      test_not_called_with_msg_chall = list(
        long = "test fails when not all library functions are called in challenge",
        sct = "test_instruction(1,test_library_function(\"yaml\")); test_instruction(1,test_library_function(\"ggvis\"))"
      )
    )
  )
)