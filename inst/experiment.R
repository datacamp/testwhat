pec <- '
# pec here
'

sol_code <- '
x <- mean(1:3)
'

stu_code <- '
x <- mean(1:3)
'

library(testwhat)
setup_state(pec = pec, sol_code = sol_code, stu_code = stu_code)

ex() %>% check_function('mean') %>% disable_highlighting() %>% check_arg('x') %>% check_equal()
