#' Test whether the student used the correct properties (ggvis exercises)
#'
#' Test whether the student used at least as many and the correct properties as the solution inside a specific
#' command and inside a specific function. By default, this function will compare the ggvis functions of both
#' student and solution. However, the teacher can also state that the definition of data can be done in other
#' functions.
#'
#' @param index  number of ggvis caommdn to be checked
#' @param funs  the function in which to look for the x and y data. If the same info is found in one function, the test passes.
#' All the functions that the teacher specifies, must be present in the students' solution! The function only looks for
#' properties inside the first mentioned function by the teacher.
#' @param props  set of properties to be checked. If not specified, all properties found in the solution or checked on. If
#' specified as an empty charactor vector (c()), only the calling of the functions will be checked on.
#' @param allow_extra  whether or not the definition of additional properties is accepted (default TRUE)
#' @param not_called_msg feedback message in case the specified function(s) was/were not found.
#' @param incorrect_msg  feedback message in case the student specified properties do not correspond with the ones in the solution.
#' @param incorrect_number_of_calls_msg  feedback message in case the student did
#' enter the same amount of commands as the solution did.
#'
#' @export
test_props <- function(index = 1,
                    funs = "ggvis",
                    props = NULL,
                    allow_extra = TRUE,
                    not_called_msg = NULL,
                    incorrect_msg = NULL,
                    incorrect_number_of_calls_msg = NULL) {

  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  init_tags(fun = "test_props")
  
  pd_stud <- get_single_pd(index = index, pd = create_student_pd(student_code = student_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  pd_sol <- get_single_pd(index = index, pd = create_solution_pd(solution_code = solution_code), incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  if(is.null(pd_stud) || is.null(pd_sol)) {
    return(FALSE)
  }
  
  # get the properties defined in the solution
  sol_exprs = get_expressions_for_function_call(funs[1], pd_sol)
  if (length(sol_exprs) != 1) {
    stop(sprintf("Function %s should only occur exactly once in command %i.", funs[1], index))
  }
  sol_props = get_all_props(funs[1], sol_exprs[[1]])
  
  if (is.null(props)) {
    props <- names(sol_props)
  } else {
    # select from sol_props the ones to check on
    sol_props = sol_props[props]
    if (any(is.na(names(sol_props)))) {
      stop(sprintf("You defined properties that are not in %s() in command %i of the solution code", funs[1], index))
    }
  }
  
  # Set up default messages
  # message if specified function was not called
  if (is.null(not_called_msg)) {
    calls <- if (length(funs) == 1) "a call" else "calls"
    not_called_msg = sprintf("Command %i of your solution should contain %s to %s.", index, calls, collapse_funs(funs))
  }
  # message if the properties or not found or set incorrectly
  if (is.null(incorrect_msg)) {
    propstr <- if (length(props) == 1) "property" else "properties"
    incorrect_msg = sprintf("In command %i of your solution, make sure to correctly define the %s %s inside %s.",
                            index, propstr, collapse_props(props), collapse_funs(funs, conn = " or "))
    if (length(props[!props  %in% c("x","y")]) > 0)
      incorrect_msg = paste(incorrect_msg, "Beware of the difference between <code>=</code> and <code>:=</code>.")
    if (!allow_extra)
      incorrect_msg = paste(incorrect_msg, "Do not define any other properties!")
    if (length(props) == 0)
      incorrect_msg = sprintf("In command %i of your solution, make sure that you do not define any properties inside %s.",
                              index, collapse_funs(funs))
  }
  
  pass <- FALSE
  keeptrying <- TRUE
  for (i in 1:length(funs)) {
    stud_exprs = get_expressions_for_function_call(funs[i], pd_stud)
    test_what(expect_that(length(stud_exprs) > 0, is_true()), feedback = not_called_msg)
    
    if (pass) # if passed already, only check on the function being present, so next loop not needed anymore
      next
    
    # possibly more expressions are available
    for (j in 1:length(stud_exprs)) {
      stud_props <- get_all_props(funs[i], stud_exprs[[j]])
      
      if (length(props) != length(stud_props) & !allow_extra) {
        # number of props specified does not correspond to function.
        # no extras were allowed ->  fail, and stop trying.
        pass <- FALSE
        keeptrying <- FALSE
        break
      }
      
      if (length(props) == 0) {
        # if no props specified, we're ok.
        pass <- TRUE
        break
      }
      
      stud_props <- stud_props[props]
      if (any(is.na(names(stud_props)))) {
        # not all properties in props are found in expression
        break
      }
      
      # check if property values correspond
      correct_val <- mapply(function(x,y) (x$value == y$value), x = sol_props, y = stud_props)
      # check if both properties were mapped or set!
      correct_mapset <- mapply(function(x,y) {
        if (is.null(x$scale)) {
          return(is.null(y$scale))
        }
        else {
          if (is.null(y$scale))
            return(FALSE)
          else
            return(x$scale == y$scale)
        }}, x = sol_props, y = stud_props)
      
      # if all passed, it's a pass
      if (all(correct_val) && all(correct_mapset)) {
        pass <- TRUE
        break
      }
    }
    
    if (!keeptrying) # if one expression contained more props, fail, and stop trying
      break
  }
  test_what(expect_true(pass), feedback = incorrect_msg)
}

