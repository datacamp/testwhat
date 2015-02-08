# #' @export
# has_arguments <- function(expected, all = TRUE) {
#
#   function(call) {
#     args <- names(call)[-1]
#     supplied <- expected %in% args
#
#     expectation(
#       if (all) all(supplied) else any(supplied),
#       "doesn't have all expected arguments",
#       "has all expected arguments"
#     )
#   }
# }
# #' @export
# expect_arguments <- function(object, expected, ..., info = NULL, label = NULL,
#                              failure_msg = NULL, success_msg = NULL) {
#   if (is.null(label)) {
#     label <- find_expr("object")
#   }
#   expect_that(object, has_arguments(expected, ...), info = info, label = label,
#               failure_msg = failure_msg, success_msg = success_msg)
# }
#
# #' @export
# test_argument <- function(name, student_call, solution_call,
#                           student_env = .GlobalEnv,
#                           solution_env = get_solution_env(),
#                           undefined_msg = NULL, incorrect_msg = NULL) {
#
#   quoted_name <- sQuote(name)
#   if (is.null(undefined_msg)) {
#     undefined_msg <- sprintf("Did you supply argument %s?", quoted_name)
#   }
#   if (is.null(incorrect_msg)) {
#     incorrect_msg <- sprintf("It looks like you didn't set the correct value for argument %s.", quoted_name)
#   }
#
#   test_that(sprintf("Argument %s is correctly specified", quoted_name), {
#     expect_that(student_call, has_arguments(name),
#                 failure_msg = undefined_msg)
#
#     student <- student_call[[name]]
#     solution <- solution_call[[name]]
#
#     if (is.name(student) || is.expression(student)) {
#       student <- eval(student, envir = student_env)
#     }
#     if (is.name(solution) || is.expression(solution)) {
#       solution <- eval(solution, envir = solution_env)
#     }
#
#     expect_that(student, is_equivalent_to(solution),
#                 failure_msg = incorrect_msg)
#   })
# }
#
# #' @importFrom evaluate parse_all
# #' @export
# split_code <- function(code, parsed = FALSE, expressions = FALSE) {
#   # Parse user code to get individual expressions
#   if (!isTRUE(parsed)) code <- parse_all(code)
#   # Convert expressions to character strings
#   exprs <- unlist(code$expr)
#   if (expressions) exprs else as.character(exprs)
# }
