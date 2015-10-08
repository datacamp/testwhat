scen <- list(
  list(
    type = "NormalExercise", 
    student = "\n    var.equiv <- 3\n    var.not_equiv <- 4", 
    solution = "\n    var.equiv <- 3\n    var.not_equiv <- 3\n    var.not_here <- 2",
    fail = list(
      test_equivalence_fail_1 = list(
        long = "handle equivalence correctly - fail 1", 
        sct = "test_object(\"var.not_equiv\")"), 
        test_undefined_msg_fail_1 = list(
          long = "test on undefined message - fail 1", 
          sct = "test_object(\"var.not_here\", undefined_msg = \"This is the undefined message\")", 
          message = "This is the undefined message"), 
      test_incorrect_msg_fail_1 = list(
        long = "test on incorrect message - fail 1",
        sct = "test_object(\"var.not_equiv\", incorrect_msg = \"This is the incorrect message\")",
        message = "This is the incorrect message")), 
    pass = list(
      test_equivalence_pass_1 = list(
        long = "handle equivalence correctly - pass 1",
        sct = "test_object(\"var.equiv\")"))), 
    list(
      type = "NormalExercise", 
      student = "\n    df.equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))\n    df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))", 
      solution = "\n    df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))\n    df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))", 
      fail = list(
        test_equal_fail_1 = list(
          long = "difference between equals and equivalent - fail 1", 
          sct = "test_object(\"df.equiv\", eq_condition = \"equal\")"), 
        test_equal_fail_2 = list(
          long = "difference between equals and equivalent - fail 2",
          sct = "test_object(\"df.not_equiv\", eq_condition = \"equal\")"), 
        test_equal_fail_3 = list(
          long = "difference between equals and equivalent - fail 3",
          sct = "test_object(\"df.not_equiv\")")), 
        pass = list(
          test_equal_pass_1 = list(
            long = "difference between equals and equivalent - pass 1",
            sct = "test_object(\"df.equiv\")"))), 
    list(
      type = "NormalExercise",
      student = "\n    var.iden <- 3\n    var.equal <- 3 + 4.4e-8",
      solution = "\n    var.iden <- 3\n    var.equal <- 3", 
      fail = list(
        test_identical_fail_1 = list(
          long = "difference between equals and identical - fail 1", 
          sct = "test_object(\"var.equal\", eq_condition = \"identical\")")), 
      pass = list(
        test_identical_pass_1 = list(
          long = "difference between equals and identical - pass 1", 
          sct = "test_object(\"var.iden\", eq_condition = \"identical\")"), 
        test_identical_pass_2 = list(
          long = "difference between equals and identical - pass 2", 
          sct = "test_object(\"var.equal\", eq_condition = \"equal\")"))), 
    list(
      type = "NormalExercise", 
      student = "var <- 3\nvar.other <- 4", 
      solution = "var <- 5\nvar.other <- 3\nvar.not_here<-7\n", 
      fail = list(
        test_eval_fail_1 = list(
          long = "test on eval parameter - fail 1", 
          sct = "test_object(\"var\")"), 
        test_eval_fail_2 = list(
          long = "test on eval parameter - fail 2", 
          sct = "test_object(\"var.not_here\", eval = FALSE)")), 
      pass = list(
        test_eval_pass_1 = list(
          long = "test on eval parameter - pass 1", 
          sct = "test_object(\"var\", eval = FALSE)"))))
