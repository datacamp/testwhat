library(testwhat)
stu_env <- new.env(parent = globalenv())
evaluate_result <- evaluate::evaluate("x <- 2", envir = stu_env)
grade_testwhat(label = "test",
               solution_code = "x <- 1",
               user_code = "x <- 2",
               check_code = "ex() %>% check_object('x') %>% check_equal()",
               envir_result = stu_env,
               evaluate_result = evaluate_result)
