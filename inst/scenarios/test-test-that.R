scen <- list()
# scen <- list(
#   list(
#     type = "NormalExercise", 
#     student = "a = 3",
#     solution = "a = 3; stoeme = \"filip\"", 
#     pass = list(
#       test_test_that_wrap = list(
#         long = "test succeeds if test_that correctly wraps test_what", 
#         sct = "test_that(\"test\", {
#                   expect_that(3, equals(3), failure_message = \"this is a test\")
#                   expect_that(\"filip\", equals(\"filip\"), \"this is a filip\")})" 
#       ),
#       test_test_wrap_func = list(
#         long = "test succeeds if we check functional test_that", 
#         sct = "test_that(\"test\", {
#                   expect_that(get(\"a\", .GlobalEnv), equals(3), failure_message = \"this is a test\")
#                   expect_that(get(\"stoeme\", .GlobalEnv), equals(\"filip\"), \"this is a filip\")})" 
#       ),
#       test_test_wrap_func = list(
#         long = "test succeeds if we check functional test_that", 
#         sct = "test_that(\"test\", {
#                   equal_to <- 3
#                   incorrect_msg <- \"this is a test\"
#                   expect_that(get(\"a\", .GlobalEnv), equals(equal_to), failure_message = incorrect_msg)
#                   filip <- \"filip\"
#                   expect_that(get(\"stoeme\", .GlobalEnv), equals(filip), \"this is a filip\")})" 
#       )
#     ),
#     fail =  list(     
#     test_test_that_wrap_fail = list(
#         long = "test succeeds if test_that correctly wraps test_what", 
#         sct = "test_that(\"test\", {
#                   expect_true(TRUE, failure_message = \"this is a test\")
#                   expect_equal(\"filip\", \"bilip\", \"this is a filip\")})",
#         message = "this is a filip"
#       ),
#       test_test_wrap_func_fail = list(
#         long = "test succeeds if we check functional test_that", 
#         sct = "test_that(\"test\", {
#                 expect_equal(get(\"a\", .GlobalEnv), 4, failure_message = \"first one\")
#                 expect_that(get(\"stoeme\", .GlobalEnv), equals(\"bilip\"), \"this is a filip\")})",
#         message = "first one"
#       ),
#       test_test_wrap_func = list(
#         long = "test succeeds if we check functional test_that", 
#         sct = "test_that(\"test\", {
#                     equal_to <- 3
#                     incorrect_msg <- \"this is a test\"
#                     expect_that(get(\"a\", .GlobalEnv), equals(equal_to), failure_message = incorrect_msg)
#                     filip <- \"bilip\"
#                     expect_that(get(\"stoeme\", .GlobalEnv), equals(filip), incorrect_msg)})",
#         message = "this is a test"
#       )
#     )
#   )
# )
