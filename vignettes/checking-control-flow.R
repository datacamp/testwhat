## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
#  # Print out the integers 1 to 10
#  for (i in 1:10) {
#    print(i)
#  }

## ------------------------------------------------------------------------
#  forloop <- ex() %>% check_for(not_found_msg = "Make sure to code a for loop in the first place!")
#  forloop %>% check_cond() %>% check_code("in\\s*1:10", missing_msg = "You can use `i in 1:10` to define your for loop.")
#  forloop %>% check_body() %>% check_function("print") %>% check_arg("x") %>% check_equal(eval = FALSE)

## ------------------------------------------------------------------------
#  # Predefined value of x
#  x <- TRUE
#  
#  # Code the if else construct
#  if (x) {
#    print("x is TRUE!")
#  } else {
#    print("x is not TRUE!")
#  }

## ------------------------------------------------------------------------
#  ex() %>% check_object("x")
#  ifelse <- ex() %>% check_if_else()
#  ifelse %>% check_cond() %>% check_code("x")
#  ifelse %>% check_if() %>% check_function("print") %>% check_arg("x") %>% check_equal()
#  ifelse %>% check_else() %>% check_function("print") %>% check_arg("x") %>% check_equal()

## ------------------------------------------------------------------------
#  if (condition1) {
#    expression
#  } else if (condition2) {
#    expression
#  } else {
#    expression
#  }

## ------------------------------------------------------------------------
#  if (condition1) {
#    expression
#  } else {
#    if (condition2) {
#  
#    } else {
#  
#    }
#  }

## ------------------------------------------------------------------------
#  ex() %>% check_if_else() %>% {
#    check_cond(.) %>% ...
#    check_if(.) %>% ...
#    check_else(.) %>% check_if_else() %>% {
#      check_cond(.) %>% ...
#      check_if(.) %>% ...
#      check_else(.) %>% ...
#    }
#  }

