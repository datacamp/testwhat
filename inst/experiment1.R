# 
# init_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "init", 
#                                             DC_PEC = "", 
#                                             DC_SOLUTION = "print(TRUE)",
#                                             DC_SCT = "test_function('print', args = 'x')",
#                                             DC_TYPE = "NormalExercise",
#                                             DC_ECHO = TRUE))))
# submit_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "submit", 
#                                               DC_TYPE = "NormalExercise",
#                                             DC_CODE = "print(list('test', 'test2'))"))))
# print(submit_output)

library(magrittr)
fun1 <- function(x) {
  print("this is fun 1")
  x
}

fun2 <- function(x) {
  print("this is fun2")
  x
}

fun3 <- function(x) {
  print("this is fun3")
  x
}

fun1(3) %>% fun2() %>% fun3()
