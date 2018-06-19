pec <- '
# pec here
'

sol_code <- '
x <- 5
'

stu_code <- '
x <- 4
'

library(testwhat)
setup_state(pec = pec, sol_code = sol_code, stu_code = stu_code)

ex() %>% check_object('x')
