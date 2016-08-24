#' Check whether the student defined a function correctly
#' 
#' @param name  The name of the function to test
#' @param function_test sub-SCT to perform on the function
#' @param body_test sub-SCT to perform on the body of the function if the
#'   sub-SCT in \code{function_test} fails. Only able to test on strings here!
#' @param undefined_msg Custom message in case the specified function
#'   was not defined
#' @param no_fundef_msg Custom message in case the function specified in \code{name} is not a function.
#' @param not_found_msg Custom feedback message if function definition was not
#'   found.
#' @param incorrect_number_arguments_msg Optional feedback message in case the 
#'   function does not have the correct number of arguments.
#' @param ... arguments to pass to the user-defined function to test result, output or error in a later stage
#' @param state the state to start from
#' @param append Whether or not to append the feedback to feedback built in previous states
#' 
#' @examples
#' \dontrun{
#' # Example:
#' my_fun <- function(a, b) { 
#'   stopifnot(is.numeric(a), is.numeric(b))
#'   a + b 
#' }
#' 
#' # SCT option 1
#' test_function_definition("my_fun",
#'                          function_test = {
#'                            test_expression_result("my_fun(1,2)")
#'                            test_expression_error("my_fun('c',3)")
#'                          }, {
#'                            test_student_typed("+")
#'                          })
#' 
#' # SCT option 2
#' fundef %>% check_fun_def("my_fun") 
#' fundef %>% check_arguments()
#' fundef %>% check_call(a = 1, b = 2) %>% check_result() %>% check_equal()
#' fundef %>% check_call(a = 'c', b = 3) %>% check_error() %>% check_equal()
#' fundef %>% check_body() %>% check_code("+")
#' }
#' 
#' @name test_fun_def

#' @rdname test_fun_def
#' @export
test_function_definition <- function(name, 
                                     function_test = NULL,
                                     body_test = NULL,
                                     undefined_msg = NULL, 
                                     incorrect_number_arguments_msg = NULL) {
  body_test <- substitute(body_test)
  function_test <- substitute(function_test)
  fundef <- ex() %>% check_fun_def(name, 
                                   undefined_msg = undefined_msg, 
                                   append = is.null(undefined_msg))
  
  fun_passed <- TRUE
  if (!is.null(function_test)) {
    fun_passed <- run_until_fail(function_test)
    fun_feedback <- get_rep()$get_feedback()
  }

  if (!fun_passed) {
    fundef %>% check_arguments(incorrect_number_arguments_msg = incorrect_number_arguments_msg, 
                               append = is.null(incorrect_number_arguments_msg))  
  }
  
  body_passed <- TRUE
  if (!is.null(body_test)) {
    oldstate <- ex()
    on.exit(tw$set(state = oldstate))
    body_state <- check_body(fundef)
    tw$set(state = body_state)
    body_passed <- run_until_fail(body_test)
    body_feedback <- get_rep()$get_feedback()
  }
  
  if (fun_passed) {
    # all good
  } else {
    if (body_passed) {
      check_that(failure(), feedback = fun_feedback)
    } else {
      check_that(failure(), feedback = body_feedback)
    }
  }
}

#' @rdname test_fun_def
#' @export
check_fun_def <- function(state, name, undefined_msg = NULL, no_fundef_msg = NULL, append = TRUE) {
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  fundef_state <- FunDefState$new(state)
  fundef_state$add_details(type = "fundef",
                          case = "defined",
                          name = name,
                          message = undefined_msg,
                          append = append,
                          pd = NULL)
  
  check_defined(name, solution_env)
  solution_object <- get(name, envir = solution_env, inherits = FALSE)
  if (!inherits(solution_object, "function")) {
    stop("%s is not a user-defined function in the solution code.", name)
  }
  
  check_that(is_true(exists(name, envir = student_env, inherits = FALSE)), feedback = fundef_state$details)
  student_object <- get(name, envir = student_env, inherits = FALSE)
  
  fundef_state$set_details(case = "correcttype",
                           message = no_fundef_msg
                           #pd = extract_function_definition(state$get("student_pd"), name))
                           )
  
  check_that(is_true(inherits(student_object, "function")),
             feedback = fundef_state$details)
  
  fundef_state$set_details(case = "correct",
                           message = NULL)
  
  fundef_state$set(name = name,
                   student_object = student_object,
                   solution_object = solution_object)
  
  return(fundef_state)
}

#' @rdname test_fun_def
#' @export
check_arguments <- function(state, incorrect_number_arguments_msg = NULL, append = TRUE) {
  stud_arguments <- as.list(formals(state$get("student_object")))
  sol_arguments <- as.list(formals(state$get("solution_object")))
  
  fundefargs_state <- FunDefArgsState$new(state)
  
  fundefargs_state$add_details(type = "fundef",
                               case = "arguments",
                               message = incorrect_number_arguments_msg,
                               append = append,
                               pd = NULL)
  
  check_that(is_equal(length(stud_arguments), length(sol_arguments)), 
             feedback = fundefargs_state$details)
  return(fundefargs_state)
}

#' @rdname test_fun_def
#' @export
check_body.FunDefState <- function(state, not_found_msg = NULL, append = TRUE, ...) {
  name <- state$get("name")
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  
  body_state <- SubState$new(state)
  
  body_state$add_details(type = "fundef",
                         case = "coded",
                         message = not_found_msg,
                         append = append,
                         pd = NULL)
  
  solution_fun_def <- extract_function_definition(solution_pd, name)
  if (is.null(solution_fun_def)) {
    stop(sprintf("The function definition of %s was not found in the solution code", name))
  }
  
  student_fun_def <- extract_function_definition(student_pd, name)
  check_that(is_false(is.null(student_fun_def)), feedback = body_state$details)
  
  body_state$set_details(type = "body", pd = student_fun_def$pd)
  decorate_state(body_state, student_fun_def, solution_fun_def)
  return(body_state)
}

#' @rdname test_fun_def
#' @export
check_call <- function(state, ...) {
  expr_state <- ExprState$new(state)
  expr_str <- gsub("list", state$get("name"), deparse(substitute(list(...))))
  expr_state$set(expr = parse(text = expr_str))
  return(expr_state)
}





