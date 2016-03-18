#' Test whether a student called a subexpression correctly. (dplyr and ggvis exercises)
#'
#' Test whether a student called a(n) (sub)expression. If yes, test for this function call
#' if the result corresponds to the subexpression called in the solution.
#'
#' When testing whether the resut is the same, small numeric differences or
#' differences in attributes are allowed.
#'
#' @param index  exercise to be checked (solution and student code should have same number of calls!)
#' @param fun  name of the function to be checked. if fun = NULL, check the entire command.
#' @param not_called_msg feedback message in case the function is not retrieved.
#' @param incorrect_msg  feedback message in case the evaluation was not the same as in the solution
#' @param incorrect_number_of_calls_msg  feedback message in case the student did
#' enter the same amount of commands as the solution did.
#'
#' @export
test_subexpr_eval <- function(index = 1, fun = NULL,
                              not_called_msg = NULL,
                              incorrect_msg = NULL,
                              incorrect_number_of_calls_msg = NULL) {

  student_code = tw$get("student_code")
  solution_code = tw$get("solution_code")
  init_tags(fun = "test_subexpr_eval")

  pd_stud <- get_single_pd(index = index, pd = create_student_pd(student_code = student_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  pd_sol <- get_single_pd(index = index, pd = create_solution_pd(solution_code = solution_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  if(is.null(pd_stud) || is.null(pd_sol)) {
    return(FALSE)
  }
  
  if(is.null(fun)) funstr = "the entire command"
  else funstr = sprintf("<code>%s()</code>",fun)

  student_exprs <- get_expressions_for_function_call(fun = fun, pd = pd_stud)
  solution_exprs <- get_expressions_for_function_call(fun = fun, pd = pd_sol)
  
  # DEFAULT MESSAGES
  if(!is.character(solution_exprs)) {
    stop(sprintf("Function %s was not found in command %i of the solution code. Check this code and your SCT.",funstr,index))
  }
  if(is.null(not_called_msg)) {
    not_called_msg = sprintf("Function %s was not called in command %i of your solution.", funstr, index);
  }
  if(is.null(incorrect_msg)) {
    incorrect_msg = sprintf("The result of %s in command %i of your solution is not as expected. Check it again.",funstr,index);
  }

  test_what(expect_true(is.character(student_exprs)), feedback_msg = not_called_msg)
  studresult = try(eval(parse(text=tail(student_exprs,n=1))))
  solresult = try(eval(parse(text = tail(solution_exprs,n=1))))

  # TAKE LAST CALLS OF EXPRESSIONS (tail(..., n = 1)) # change this?
  test_what(expect_true(isTRUE(try(all.equal(studresult, solresult)))), feedback_msg = incorrect_msg)
}
