# Scenarios for test object
scenarios <- new.env()

# Scenario 1: one object is equivalent, the other isn't
scenarios$test_equivalence <- new.env()

scenarios$test_equivalence$pre_ex <- ''
scenarios$test_equivalence$student <- '
var.equiv <- 3
var.not_equiv <- 4'
scenarios$test_equivalence$solution <-'
var.equiv <- 3
var.not_equiv <- 5'

# Scenario 2: test difference equal and equivalent
scenarios$test_equal <- new.env()

scenarios$test_equal$pre_ex <- ''
scenarios$test_equal$student <- '
df.equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))
df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))'
scenarios$test_equal$solution <- '
df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))
df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))'

# Scenario 3: test difference between identical and equal
scenarios$test_identical <- new.env()

scenarios$test_identical$pre_ex <- ''
scenarios$test_identical$student <- '
var.iden <- 4
var.equal <- 3 + 4.4e-8'
scenarios$test_identical$solution <- '
var.iden <- 4
var.equal <- 3'

# Scenario 4: test on eval argument
scenarios$test_eval <- new.env()

scenarios$test_eval$pre_ex <- ''
scenarios$test_eval$student <- '
var <- 3
var.other <- 4'
scenarios$test_eval$solution <- '
var <- 5
var.other <- 3'