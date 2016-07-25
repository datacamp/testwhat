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
