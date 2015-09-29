scen <- list(
  list(
    type = "NormalExercise", 
    student = "df.equiv <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equiv <- data.frame(a = c(1, 4, 3), b = c(\"a\", \"b\", \"c\"))", 
    solution = "df.equiv <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"), c = c(1, 1, 1))\n  df.not_here <- data.frame(a = c(7, 8, 9), b = c(\"a\", \"b\", \"c\"\n))", 
    pass = list(
      test_equivalent_pass = list(
        long = "test succeeds if data.frame is equivalent", 
        sct = "test_data_frame(\"df.equiv\")"
      ),
      test_specific_columns_pass = list(
        long = "test succeeds if data.frame is equivalent in certain column", 
        sct = "test_data_frame(\"df.not_equiv\", columns = \"b\")"
      )
    ),
    fail = list(
      test_equivalent_fail = list(
        long = "test fails if check data.frame is not equivalent", 
        sct = "test_data_frame(\"df.not_equiv\")"
      ),
      test_specific_columns_fail = list(
        long = "test fail if data.frame is not equivalent in certain column", 
        sct = "test_data_frame(\"df.not_equiv\", columns = \"a\")"
      ),
      test_undefined_msg = list(
        long = "test undefined msg if data.frame is not there", 
        sct = "test_data_frame(\"df.not_here\", undefined_msg = \"data frame not here\")",
        message = "data frame not here"
      ),
      test_undefined_cols_msg = list(
        long = "test undefined cols msg if data.frame column is not there", 
        sct = "test_data_frame(\"df.not_equiv\", columns = c(\"c\", \"b\"), undefined_cols_msg = \"data frame column not here\")",
        message = "data frame column not here"
      ),
      test_incorrect_msg = list(
        long = "test undefined cols msg if data.frame column is not there", 
        sct = "test_data_frame(\"df.not_equiv\", columns = c(\"a\", \"b\"), incorrect_msg = \"data frame column not correct\")",
        message = "data frame column not correct"
      )
    )
  ),
  list(
    type = "NormalExercise", 
    student = "df.equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"\n))\n  rownames(df.not_equal) <- c(\"one\", \"two\", \"three\")", 
    solution = "df.equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_equal <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"\n))\n  rownames(df.not_equal) <- c(\"one\", \"oops\", \"three\")", 
    pass = list(
      test_equivalent_pass_not_equal = list(
        long = "test succeeds if data.frame is equivalent while not being equal", 
        sct = "test_data_frame(\"df.not_equal\")"
      ),
      test_equal_pass = list(
        long = "test succeeds if data.frame is equal", 
        sct = "test_data_frame(\"df.equal\", eq_condition = \"equal\")"
      )
    ),
    fail = list(
      test_equal_fail = list(
        long = "test fails if data.frame is not equal", 
        sct = "test_data_frame(\"df.not_equal\", eq_condition = \"equal\")"
      )
    )
  ),
  list(
    type = "NormalExercise", 
    student = "df.iden <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_iden <- data.frame(a = c(1 + 4.4e-9, 2, 3), b = c(\"a\", \"b\", \"c\"\n))", 
    solution = "df.iden <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"))\n  df.not_iden <- data.frame(a = c(1, 2, 3), b = c(\"a\", \"b\", \"c\"\n))", 
    pass = list(
      test_equivalent_pass_not_iden = list(
        long = "test succeeds if data.frame is equivalent while not being identical", 
        sct = "test_data_frame(\"df.not_iden\")"
      ),
      test_iden_pass = list(
        long = "test succeeds if data.frame is identical", 
        sct = "test_data_frame(\"df.iden\", eq_condition = \"identical\")"
      )
    ),
    fail = list(
      test_iden_fail = list(
        long = "test fails if data.frame is not identical", 
        sct = "test_data_frame(\"df.not_iden\", eq_condition = \"identical\")"
      )
    )
  )
)