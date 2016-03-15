#' Test whether the student correctly defined axis properties (ggvis exercises)
#'
#' Test whether the student correctly assigned axis properties. The student's and solution code is automatically compared to
#' one another for specific properties of the axes.
#'
#' This test is implemented using \code{\link{test_that}}.
#'
#' @param index  exercise to be checked (solution and student code should have same number of calls!)
#' properties inside the first mentioned function by the teacher.
#' @param type  which axis to check (x or y). Only one axis is tested at the same time.
#' @param props  set of axis properties to be checked. If not specified, all properties found in the solution or checked on.
#' @param not_called_msg feedback message in case the specified axis type (x or y) was not found
#' @param incorrect_msg  feedback message in case the axes properties defined by the student did not correspond with the one of the solution.
#' @param incorrect_number_of_calls_msg  feedback message in case the student did
#' enter the same amount of commands as the solution did.
#'
#' @export
test_axis <- function(index = 1,
                       type = NULL,
                       props = NULL,
                       not_called_msg = NULL,
                       incorrect_msg = NULL,
                       incorrect_number_of_calls_msg = NULL) {
  
  student_code = tw$get("student_code")
  solution_code = tw$get("solution_code")
  on.exit({
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
  })
  init_tags(fun = "test_axis")

  if(is.null(type)) {
    stop("Argument 'type' cannot be empty!")
  }

  pd_stud <- get_single_pd(index = index, pd = create_student_pd(student_code = student_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  pd_sol <- get_single_pd(index = index, pd = create_solution_pd(solution_code = solution_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  if(is.null(pd_stud) || is.null(pd_sol)) {
    return(FALSE)
  }
  
  # get the add_axis commands
  stud_exprs = get_expressions_for_function_call("add_axis", pd_stud)
  sol_exprs = get_expressions_for_function_call("add_axis", pd_sol)

  # set up default messages
  if(is.null(not_called_msg)) {
    not_called_msg = sprintf('In command %i, you did not add properties for the %s axis using add_axis("%s", ... )', index, type, type)
  }

  sol_expr = get_expression_by_type(sol_exprs, type)
  if(is.null(sol_expr)) {
    stop(sprintf("The solution did not specify an axis of type %s in command %i.", type, index))
  }
  
  stud_expr = get_expression_by_type(stud_exprs, type)
  test_what(expect_that(is.null(stud_expr), is_false()), feedback_msg = not_called_msg)

  for(p in 1:length(props)) {
    if(is.null(incorrect_msg)) {
      incorrect_msg <- sprintf("In command %i of your solution, the %s property of the %s axis is not correct.", index, props[p], type)
    }
    tw$set(student_code = stud_expr)
    tw$set(solution_code = sol_expr)
    test_function("add_axis", args = props[p],
                  student_code = stud_expr, solution_code = sol_expr, 
                  incorrect_msg = incorrect_msg)
  }
}

get_expression_by_type = function(expressions, type) {
  expr = NULL
  for(i in 1:length(expressions)) {
    args <- arguments_for_expression(expressions[i], "add_axis")
    if(args["type"] == type) {
      expr = expressions[i]
      break
    }
  }
  return(expr)
}

arguments_for_expression <- function(expression, fun) {
  arguments = match.call(get(fun), call = parse(text = expression))[-1]
  arguments_list = as.list(arguments)
  names = names(arguments_list)
  arguments_list = as.character(arguments_list)
  names(arguments_list) = names
  return(arguments_list)
}
