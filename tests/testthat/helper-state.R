setup_state <- function(STU_CODE, SOL_CODE, PEC = character()) {

  sol_env <- new.env()
  stu_env <- new.env()
  
  evaluate::evaluate(PEC,      envir=sol_env)
  evaluate::evaluate(SOL_CODE, envir=sol_env)
  evaluate::evaluate(PEC,      envir=stu_env)
  evaluate::evaluate(STU_CODE, envir=stu_env)
  
  testwhat.base::tw$clear()
  
  state <- testwhat.base::RootState$new(
    pec = PEC,
    student_code = STU_CODE,
    student_pd = testwhat.base::build_pd(STU_CODE),
    student_env = stu_env,
    solution_code = SOL_CODE,
    solution_pd = testwhat.base::build_pd(SOL_CODE),
    solution_env = sol_env,
    output_list = list(),
    test_env = new.env(parent=globalenv())
  )
  
  # testwhat will access the reporter and state from the tw object
  rep <- testwhat.base::DC_reporter$new()
  testwhat.base::tw$set(state = state, reporter = rep, stack = TRUE)
  testwhat.base::tw$get("state")
}

