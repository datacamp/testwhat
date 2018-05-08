## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
#  # vec is predefined for you
#  vec <- c(1, 2, 3, 4, 5, 6)
#  
#  # Calculate result
#  result <- mean(vec)

## ------------------------------------------------------------------------
#  check_correct(ex() %>% check_object("result") %>% check_equal(),
#                {
#                  ex() %>% check_error()
#                  ex() %>% check_function("mean") %>% check_arg("x") %>% check_equal()
#                })

## ------------------------------------------------------------------------
#  print(4)

## ------------------------------------------------------------------------
#  check_or(ex() %>% check_output_expr('4'),
#           ex() %>% check_output_expr('5'),
#           ex() %>% check_output_expr('6'))
#  success_msg("Nice job!")

## ------------------------------------------------------------------------
#  check_or(ex() %>% check_output_expr('4'),
#           ex() %>% check_output_expr('5'),
#           ex() %>% check_output_expr('6'),
#           incorrect_msg = "Just print out either 4, 5, or 6!")
#  success_msg("Nice job!")

