scen <- list(
  list(
    type = "NormalExercise", 
    pre_exercise_code = "if (file.exists(\"testing.txt\")) file.remove(\"testing.txt\")",
    student = "\n  write(\"testing\", file = \"testing.txt\")",
    solution = "\n  write(\"testing\", file = \"testing.txt\")", 
    pass = list(
      test_file_exists_pass = list(
        long = "test succeeds if file exists", 
        sct = "test_file_exists(\"testing.txt\")\nfile.remove(\"testing.txt\")" 
      )
    ),
    fail = list(
      test_file_exists_fail = list(
        long = "test fails if file does not exist", 
        sct = "test_file_exists(\"this_filename_can_not_possibly_exist.txt\")"
      ),
      test_file_exists_incorrect_msg = list(
        long = "test fails if file does not exist", 
        sct = "test_file_exists(\"this_filename_can_not_possibly_exist.txt\", incorrect_msg = \"You crazy moth...\")",
        message = "You crazy"
      )
    )
  )
)