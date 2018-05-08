## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
#  # Print out your name
#  "My name is DataCamp"

## ------------------------------------------------------------------------
#  ex() %>% check_output("[M|m]y name is [a-zA-Z]+?", missing_msg = "Have you printed out your name?")

## ------------------------------------------------------------------------
#  # Print out the fourth row of mtcars
#  mtcars[4, ]

## ------------------------------------------------------------------------
#  ex() %>% check_output_expr("mtcars[4, ]", missing_msg = "Have you used `[4, ]` to print out the fourth row of `mtcars`?")

