## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
#  # Define my_fun
#  my_fun <- function(a, b) {
#  	abs(a) + abs(b)
#  }

## ------------------------------------------------------------------------
#  ex() %>% check_fun_def("my_fun") %>% {
#    check_arguments(.)
#    check_call(., 1, 2) %>% check_result()
#    check_call(., -1, 2) %>% check_result()
#    check_call(., 1, -2) %>% check_result()
#    check_call(., -1, -2) %>% check_result()
#    check_body(.) %>% {
#      check_function(., "abs", index = 1)
#      check_function(., "abs", index = 2)
#    }
#  }

