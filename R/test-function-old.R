# #' Test whether a student correctly called a function
# #'
# #' Test whether a student called a function with certain arguments as least as
# #' many times as in a sample solution.
# #' 
# #' @param name  name of the function to test.
# #' @param args  character vector of argument names that the student should have
# #' supplied in the function calls.  If no argument names are given, it is only
# #' tested whether the student called the function at least as many times as in
# #' the sample solution.
# #' @param ignore character vector of argument names that should not be tested
# #' (useful in combination with \code{allow_extra = FALSE} to allow certain
# #' arguments to be ignored, but not others).
# #' @param allow_extra  indicates whether extra arguments not specified by
# #' \code{args} or \code{ignore} are allowed in the student's function calls.
# #' @param eval  logical vector indicating whether the corresponding argument
# #' should be evaluated before testing.  Setting this to \code{FALSE} can be
# #' useful, e.g., to test whether the student supplied a large predefined
# #' object, as only the corresponding \code{\link{name}} is compared in this
# #' case (use with care!).
# #' @param eq_condition  character vector indicating how to perform the
# #' comparison for each argument. See \code{\link{test_object}}
# #' @param not_called_msg  feedback message in case the student did not call the
# #' function at least as often as in the solution code.
# #' @param incorrect_msg  feedback message in case the student did not call the
# #' function with the same argument values as in the sample solution.  If
# #' there are multiple function calls in the sample solution, a vector of
# #' feedback messages can be supplied.
# #' 
# #' @examples
# #' \dontrun{
# #' # Suppose the solution contains: mean(1:3, na.rm = TRUE)
# #' # To test this submission, provide the following in the sct
# #' test_function("mean", c("x", "na.rm"))
# #' }
# #' 
# #' @export
# test_function_old <- function(name, args = NULL, 
#                           ignore = NULL,
#                           allow_extra = TRUE,
#                           eval = TRUE,
#                           eq_condition = "equivalent",
#                           not_called_msg = NULL, incorrect_msg = NULL) {
#   
#   student_env <- tw$get("student_env")
#   solution_env <- tw$get("solution_env")
#   student_pd <- tw$get("student_pd")
#   solution_pd <- tw$get("solution_pd")
#   init_tags(test = "test_function")
#   
#   if (is.null(name)) {
#     stop("Argument \"name\" is missing, with no default")
#   }
#   
#   n_args <- length(args)
#   eval <- rep(eval, length.out = n_args)
#   eq_condition <- rep(eq_condition, length.out = n_args)
#   
#   eq_fun <- lapply(eq_condition, function(cond) {
#     switch(cond, equivalent = expect_equivalent,
#                  identical = expect_identical,
#                  equal = expect_equal,
#                  like = expect_like,
#                  stop("invalid equality condition"))
#   })
#   
#   arg_text <- build_arg_text(n_args, args)
#   
#   # Find all function calls in the student and solution code
#   student_calls <- find_function_calls(name, student_pd, student_env)
#   solution_calls <- find_function_calls(name, solution_pd, solution_env)
#   
#   if (n_args > 0) {
#     # Only use function calls with the specified arguments
#     keep_student <- have_arguments(student_calls, args, ignore, allow_extra)
#     student_calls <- student_calls[keep_student]
#     keep_solution <- have_arguments(solution_calls, args, ignore, allow_extra)
#     solution_calls <- solution_calls[keep_solution]
#   }
#   
#   n_student_calls <- length(student_calls)
#   real_n_student_calls <- n_student_calls
#   n_solution_calls <- length(solution_calls)
#     
#   # Test if there are at least as many student function calls as solution
#   # function calls
#   if (is.null(not_called_msg)) {
#     not_called_msg <- build_not_called_msg(n_solution_calls, name, arg_text, additionaltext)
#   }
#   
#   test_what(expect_more_than(n_student_calls+1, n_solution_calls), not_called_msg)
#     
#   ## If supplied, test arguments
#   if (n_args > 0) {
#     eq_fun <- lapply(eq_condition, function(cond) {
#       switch(cond, equivalent = .equivalent, equal = .equal,
#                         identical = identical, stop("invalid equality condition"))
#     })
#     
#     # Loop over the solution function calls:
#     # Extract the specified arguments from current solution function call and
#     # test if there exists a student function call with the same values
#     if (is.null(incorrect_msg)) {
#       incorrect_msg <- build_incorrect_msg(n_solution_calls, n_args, arg_text, name, additionaltext)
#     }
#     incorrect_msg <- rep(incorrect_msg, length.out = n_solution_calls)
#     for (i in seq_len(n_solution_calls)) {
#       found_correct <- FALSE
#       # Extract the specified arguments from current solution function call
#       solution_args <- extract_arguments(solution_calls[[i]], args, eval,
#                                          env = solution_env)
#       
#       # Loop over the student function calls:
#       # Extract the specified arguments from current student function call
#       # and test if they are the same as the values in the solution
#       for (j in seq_len(n_student_calls)) {
#         student_args <- try(
#           extract_arguments(student_calls[[j]], args, eval, env = student_env),
#           silent = TRUE)
#         if (inherits(student_args, "try-error")) {
#           test_what(fail(),
#                     incorrect_msg[[i]])
#         }
#         correct <- logical(n_args)
#         
#         for (n in 1:n_args) {
#           correct[n] <- eq_fun[[n]](student_args,solution_args)
#         }
# 
#         correct <- all(correct)
#         
#         if (correct) {
#           student_calls[[j]] <- NULL
#           n_student_calls <- length(student_calls)
#           found_correct <- TRUE
#           break
#         }
#       }
# 
#       test_what(expect_true(found_correct), incorrect_msg[[i]])
#     }
#   }
# }
# 
