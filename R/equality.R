invalid_eq_condition <- "eq_condition should be either 'equivalent', 'equal' or 'identical'."

# Check equality with a specified equality condition
is_equal <- function(x, y, condition = "equivalent") {
  eq_fun <- switch(condition, 
                   equivalent = .equivalent, 
                   equal = .equal,
                   identical = identical, 
                   stop("invalid equality condition"))
  eq_fun(x, y)
}

.equivalent <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
.equal <- function(x, y) isTRUE(all.equal(x, y))
