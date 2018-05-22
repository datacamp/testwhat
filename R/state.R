
#' @importFrom testwhat.base ChildState
NULL

CallState <- R6::R6Class("FunctionState", inherit = ChildState, private = list(student_calls = NULL, solution_call = NULL))
FunctionState <- R6::R6Class("FunctionState", inherit = CallState)
OperationState <- R6::R6Class("OperationState", inherit = CallState)

CallResultState <- R6::R6Class("CallResultState", inherit = CallState)
FunctionResultState <- R6::R6Class("FunctionResultState", inherit = CallResultState)
OperationResultState <- R6::R6Class("OperationResultState", inherit = CallResultState)

ArgumentState <- R6::R6Class("ArgumentState", inherit = ChildState, private = list(student_args = NULL, solution_arg = NULL))

ObjectState <- R6::R6Class("ObjectState", inherit = ChildState, private = list(name = NULL, student_object = NULL, solution_object = NULL))
ObjectColumnState <- R6::R6Class("ObjectColumnState", inherit = ObjectState)
ObjectElementState <- R6::R6Class("ObjectElementState", inherit = ObjectState)

FunDefState <- R6::R6Class("FunDefState", inherit = ChildState, private = list(name = NULL, student_object = NULL, solution_object = NULL))
FunDefArgsState <- R6::R6Class("FunDefArgsState", inherit = FunDefState)
ExprState <- R6::R6Class("ExprState", inherit = ChildState, private = list(expr = NULL))

ExprEvalState <- R6::R6Class("ExprState", inherit = ChildState, private = list(student_object = NULL, solution_object = NULL))
ExprResultState <- R6::R6Class("ExprResultState", inherit = ExprEvalState)
ExprOutputState <- R6::R6Class("ExprOutputState", inherit = ExprEvalState)
ExprErrorState <- R6::R6Class("ExprErrorState", inherit = ExprEvalState)

ControlState <- R6::R6Class("ControlState", inherit = ChildState, private = list(student_struct = NULL, solution_struct = NULL))

SubState <- R6::R6Class("SubState", inherit = ChildState)

RegexState <- R6::R6Class("RegexState", inherit = ChildState)
FileState <- R6::R6Class("FileState", inherit = ChildState)
ErrorState <- R6::R6Class("ErrorState", inherit = ChildState)

MarkdownState <- R6::R6Class("MarkdownState", inherit = ChildState,
                             private = list(student_ds = NULL,
                                            solution_ds = NULL,
                                            student_ds_part = NULL,
                                            solution_ds_part = NULL,
                                            inline_number = NULL,
                                            chunk_number = NULL))
# Get the main state
ex <- function() {
  return(tw$get("state"))
}

decorate_state <- function(state, stud, sol, el = NULL) {
  if (is.null(el)) {
    state$set(student_pd = stud$pd,
              solution_pd = sol$pd,
              student_code = stud$code,
              solution_code = sol$code)
  } else {
    state$set(student_pd = stud[[el]]$pd,
              solution_pd = sol[[el]]$pd,
              student_code = stud[[el]]$code,
              solution_code = sol[[el]]$code)
  }
}

#' Functions to override solution code (and parse data) and variables in the solution environment.
#' 
#' Produces a new state with a custom solution code. Mostly useful inside
#' \code{check_or}, if you want to test for different cases.
#' 
#' @param state the state to create a substate from
#' @param code the solution code to put into the state
#' @param ... named environment variables to add to or override in the solution environment
#' 
#' @name override

#' @rdname override
#' @importFrom testwhat.base build_pd
#' @export
override_solution <- function(state, code = NULL, ...) {
  sub_state <- SubState$new(state)
  if (!is.null(code)) {
    sub_state$set(solution_code = code, solution_pd = build_pd(code))
  }
  env = list(...)
  if (!is.null(env)) {
    sub_state$set(solution_env = new.env(parent = globalenv()))
    for (el in ls(envir = state$get("solution_env"))) {
      assign(el, get(el, envir = state$get("solution_env")), envir = sub_state$get("solution_env"))
    }
    for (i in seq_along(env)) {
      name <- names(env)[i]
      val <- env[[i]]
      assign(name, val, envir = sub_state$get("solution_env"))
    }
  }
  return(sub_state)
}

#' @rdname override
#' @export
override_solution_code <- function(state, code) {
  override_solution(state, code = code)
}

#' @rdname override
#' @export
override_solution_env <- function(state, ...) {
  override_solution(state, code = NULL, ...)
}




