invalid_eq_condition <- "eq_condition should be either 'equivalent', 'equal' or 'identical'."

# Check equality with a specified equality condition
is_equal <- function(x, y, eq_condition = "equivalent") {
  eq_fun <- switch(eq_condition,
                   equivalent = function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE)),
                   equal = function(x, y) isTRUE(all.equal(x, y)),
                   identical = identical,
                   stop(invalid_eq_condition))
  eq_fun(x, y)
}

check_attr <- function(eq_condition) {
  switch(eq_condition,
         equivalent = FALSE,
         equal = TRUE,
         identical = TRUE,
         stop(invalid_eq_condition))
}