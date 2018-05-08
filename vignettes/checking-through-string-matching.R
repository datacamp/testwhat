## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
#  # Write the sentence in quotes
#  "R is Rsome!"

## ------------------------------------------------------------------------
#  ex() %>% check_code("^\"[r|R] is [r|R]some!?\"$",
#                      not_typed_msg = "Have you correctly written the sentence `R is Rsome!`?")
#  success_msg("Nice job!")

## ------------------------------------------------------------------------
#  # Create the correct SQL expression
#  x <- "SELECT posts FROM tweets WHERE n_char > 10"

## ------------------------------------------------------------------------
#  ex() %>% check_code(c("select posts from tweets where n_char > 10",
#                        "SELECT posts FROM tweets WHERE n_char > 10",
#                        "SELECT posts from tweets where n_char > 10"),
#                      fixed = TRUE,
#                      not_typed_msg = "Have you correctly coded the SQL query?")
#  success_msg("Great job!")

