context("highlighting")

test_that("disable_highlighting() works", {
  
  lst <- list(DC_CODE = "if (3 > 2) round(1)",
              DC_SOLUTION = "if (3 > 2) round(2)")
  
  lst$DC_SCT <- "ex() %>% check_if_else() %>% check_if() %>% check_function('round') %>% check_arg('x') %>% check_equal()"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1, 18, 18)
  
  # fall back on body highlighting
  lst$DC_SCT <- "ex() %>% check_if_else() %>% check_if() %>% check_function('round') %>% disable_highlighting() %>% check_arg('x') %>% check_equal()"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1, 12, 19)
  
  # disableing is preserved throughout chain
  lst$DC_SCT <- "ex() %>% disable_highlighting() %>% check_if_else() %>% check_if() %>% check_function('round') %>% check_arg('x') %>% check_equal()"
  output <- test_it(lst)
  fails(output)
  no_line_info(output)
})
