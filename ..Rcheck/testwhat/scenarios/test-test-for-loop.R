scen <- list(
  list(
    type = "NormalExercise", 
    student = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }", 
    solution = "\n  for (i in 1:3) {\n    rnorm(10,i)\n  }", 
    pass = list(
      test_basic_check_pass_1 = list(
        long = "check whether basic test works - pass 1", 
        sct = "test_for_loop()"))), 
    list(
      type = "NormalExercise", 
      student = "",
      solution = "\n  for (i in 1:3) {\n    rnorm(10,i)\n  }", 
      fail = list(
        test_basic_check_fails_fail_1 = 
          list(
            long = "check whether basic test fails if there is no for statement - fail 1", 
            sct = "test_for_loop()"))), 
  list(type = "NormalExercise", 
       student = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }", 
        solution = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }", 
       pass = list(test_cond_test_pass_1 = list(
         long = "check if cond_test is checked correctly - pass 1", 
         sct = c("test_for_loop(cond_test = {", 
                 "    test_object(\"i\")", 
                 "})")
         ))
       ), 
  list(
    type = "NormalExercise", 
    student = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }", 
    solution = "\n  for (i in 1:3) {\n    rpois(10,i)\n  }", 
    fail = list(
      test_cond_test_fails_fail_1 = list(
        long = "check if cond_test fails correctly - fail 1", 
        sct = c("test_for_loop(cond_test = {", 
                "    test_object(\"i\")", 
                "})"
                )
        ),
      test_cond_test_fails_fail_2 = list(
        long = "check if cond_test fails correctly - fail 2", 
            sct = c("test_for_loop(cond_test = {", 
                    "    test_student_typed(\"retteketet\")", 
                    "})")), 
      test_cond_test_msg_fail_1 = list(
        long = "check if cond_test fail message is displayed correctly - fail 1", 
        sct = c("test_for_loop(cond_test = {", 
                "    test_object(\"i\", incorrect_msg = \"Wrong condition\")",
                "})"), 
        message = "Wrong condition"))),
  list(
    type = "NormalExercise", 
    student = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }", 
    solution = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }", 
    pass = list(
      test_expr_test_pass_1 = list(
        long = "check if expr_test is checked correctly - pass 1", 
        sct = c("test_for_loop(expr_test = {", 
                "    test_function(\"rpois\", c(\"n\", \"lambda\"))", 
                "})")
        ), 
      test_expr_test_pass_2 = list(long = "check if expr_test is checked correctly - pass 2", 
                                   sct = c("test_for_loop(expr_test = {", 
                                           "    test_function(\"rpois\", c(\"n\", \"lambda\"))", "    test_object(\"i\")", "})")))), list(type = "NormalExercise", 
        student = "\n  for (i in 1:10) {\n    rpois(3,i)\n  }", solution = "\n  for (i in 1:10) {\n    rpois(10,i)\n    rnorm(10,i)\n  }", 
        fail = list(test_expr_test_fails_fail_1 = list(long = "check if expr_test fails correctly - fail 1", 
            sct = c("test_for_loop(expr_test = {", "    test_function(\"rpois\", c(\"n\", \"lambda\"))", "})")), 
            test_expr_test_fails_fail_2 = list(long = "check if expr_test fails correctly - fail 2", sct = c("test_for_loop(expr_test = {", 
                "    test_function(\"rnorm\")", "})")), test_expr_test_msg_fail_1 = list(long = "check if cond_test fail message is displayed correctly - fail 1", 
                sct = c("test_for_loop(expr_test = {", "    test_function(\"rpois\", c(\"n\", \"lambda\"), incorrect_msg = \"Wrong expression\")", 
                  "})"), message = "Wrong expression"))), list(type = "NormalExercise", student = "\n  for (i in 3:8) {\n    rpois(2,i)\n  }\n  a <- \"some code here\"\n  for (n in 3:5) {\n    rnorm(5, n*n)\n  }", 
        solution = "\n  for (i in 1:10) {\n    rpois(10,i)\n  }\n  for (n in 3:5) {\n    rnorm(5, n*n)\n  }", 
        pass = list(test_index_pass_1 = list(long = "check if index is checked correctly - pass 1", sct = c("test_for_loop(2, cond_test = {", 
            "    test_object(\"n\")", "}, expr_test = {", "    test_function(\"rnorm\", c(\"n\"))", 
            "})"))), fail = list(test_index_fail_1 = list(long = "check if index is checked correctly - fail 1", 
            sct = c("test_for_loop(1, expr_test = {", "    test_function(\"rpois\", c(\"n\", \"lambda\"))", "})")), 
            test_not_found_msg_fail_1 = list(long = "check if not_found_msg is displayed correctly - fail 1", 
                sct = "test_for_loop(3, not_found_msg = \"Too much looooooops\")", message = "Too much looooooops"))))
