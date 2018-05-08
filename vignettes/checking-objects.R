## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
#  # Create the variable x
#  x <- 15

## ------------------------------------------------------------------------
#  ex() %>% check_object("x") %>% check_equal()

## ------------------------------------------------------------------------
#  # Create an arbitrary object x
#  x <- 123

## ------------------------------------------------------------------------
#  ex() %>% check_object("x")
#  success_msg("Arbitrary, I like that.")

## ------------------------------------------------------------------------
#  # Create the list l
#  l <- list(a = 2, b = 3, c = 4)

## ------------------------------------------------------------------------
#  ex() %>% check_object("l") %>% check_equal()

## ------------------------------------------------------------------------
#  l <- list(d = 2, e = 3, f = 4)

## ------------------------------------------------------------------------
#  ex() %>% check_object("l") %>% check_equal(eq_condition = "equal")

## ------------------------------------------------------------------------
#  l <- list(d = 2, e = 3, f = 4)

## ------------------------------------------------------------------------
#  x = 123

## ------------------------------------------------------------------------
#  x = "123"

## ------------------------------------------------------------------------
#  ex() %>% check_object('x') %>% check_equal()

## ------------------------------------------------------------------------
#  # A dataframe df is available
#  df <- data.frame(name = c("vincent", "lisa", "robin"),
#                   count1 = c(4, 7, 2),
#                   count2 = c(5, 8, 10))
#  
#  # Add a column tot, sum of count1 and count2
#  df$tot <- df$count1 + df$count2

## ------------------------------------------------------------------------
#  ex() %>% check_object("df", undefined_msg = "Make sure to not remove `df`!") %>%
#    check_column("tot", col_missing_msg = "Have you added the column `tot` to `df`?") %>%
#    check_equal(incorrect_msg = "Have you correctly calculated the column `tot` based on `count1` and `count2`?")

