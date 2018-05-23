## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ------------------------------------------------------------------------
# Round pi to three digits
round(pi, 3)

## ------------------------------------------------------------------------
fun <- ex() %>% check_function("round") %>% {
  check_arg(., "x") %>% check_equal()
  check_arg(., "digits") %>% check_equal()
}

## ------------------------------------------------------------------------
fun <- ex() %>% check_function("round", not_called_msg = "Have you used `round()` to round `pi`?")
fun %>%
  check_arg("x", arg_not_specified_msg = "Have you specified the number to round?") %>%
  check_equal("Have you correctly specified that `round()` should round `pi`?")
fun %>%
  check_arg("digits", arg_not_specfied_msg = "Have you specified to how many digits the number should be rounded?") %>%
  check_equal("Have you correctly set the `digits` argument to 3?")

## ------------------------------------------------------------------------
fun <- ex() %>% check_function("round")
fun %>% check_arg("x") %>% check_equal(eval = FALSE)
fun %>% check_arg("digits") %>% check_equal()

## ------------------------------------------------------------------------
# Call round on pi
round(pi, 3)

# Call round on e
round(exp(1), 3)

## ------------------------------------------------------------------------
ex() %>% check_function("round", index = 1) %>% check_arg("x") %>% check_equal()
ex() %>% check_function("round", index = 2) %>% check_arg("x") %>% check_equal()

## ------------------------------------------------------------------------
# Call round on pi
round(pi, 3)

# Call round on e
round(exp(1), 3)

## ------------------------------------------------------------------------
ex() %>% check_function("round") %>% check_arg("x") %>% check_equal()

## ------------------------------------------------------------------------
# Call round on pi
round(pi, 3)

# Call round on e
round(exp(2), 3)

## ------------------------------------------------------------------------
ex() %>% check_function("round", index = 1) %>% check_arg("x") %>% check_equal()
ex() %>% check_function("round", index = 2) %>% check_arg("x") %>% check_equal()

## ------------------------------------------------------------------------
print(1.1234)

## ------------------------------------------------------------------------
print("1.1234")

## ------------------------------------------------------------------------
grepl(pattern = "a{2}", x = "aabb")
grepl(pat = "a{2}", x = "aabb")
grepl("a{2}", x = "aabb")
grepl("a{2}", "aabb")
grepl(x = "aabb", pattern = "a{2}")
grepl(x = "aabb", "a{2}")

## ------------------------------------------------------------------------
grepl(pattern = "a{2}", x = "aabb")

## ------------------------------------------------------------------------
sum(1, 2, 3, 4, NA, na.rm = TRUE)

## ------------------------------------------------------------------------
ex() %>% check_function("sum") %>% {
    check_arg(., "...") %>% check_equal()
    check_arg(., "na.rm") %>% check_equal()
}

## ------------------------------------------------------------------------
sum(c(1, 2, 3, 4, NA), na.rm = TRUE)

## ------------------------------------------------------------------------
test_output_contains("10", incorrect_msg = "Did you correctly print out the sum?")

## ------------------------------------------------------------------------
df <- data.frame(time = seq(0, 2*pi, 0.01))
df$res <- sin(df$time)

# create a plot of res vs time
plot(df$time, df$res)

## ------------------------------------------------------------------------
test_function("plot", args = c("x", "y"))

## ------------------------------------------------------------------------
test_or({
  fun <- ex() %>% check_function('plot')
  fun %>% check_arg('x') %>% check_equal()
  fun %>% check_arg('y') %>% check_equal()
}, {
  fun <- ex() %>% override_solution('plot(res ~ time, data = df)') %>% check_function('plot')
  fun %>% check_arg('formula') %>% check_equal()
  fun %>% check_arg('data') %>% check_equal()
}, {
  ex() %>% override_solution('plot(df$res ~ df$time)') %>% check_function('plot') %>% check_arg('formula') %>% check_equal()
})

## ------------------------------------------------------------------------
# Calculate sum of vector 1 to 5
sum(1:5)

## ------------------------------------------------------------------------
ex() %>% check_function("sum") %>% check_result() %>% check_equal()

