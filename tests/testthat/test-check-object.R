context("check_object")

test_that("check_object step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "ex() %>% check_object('x') %>% check_equal()"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define")

  lst$DC_CODE <- "x <- 4"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of")

  lst$DC_CODE <- "x <- 5"
  output <- test_it(lst)
  passes(output)
})

test_that("check_object step by step - custom - 1", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "ex() %>% check_object('x', undefined_msg = 'undef') %>% check_equal(incorrect_msg = 'incorr')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Undef")

  lst$DC_CODE <- "x <- 4"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorr")

  lst$DC_CODE <- "x <- 5"
  output <- test_it(lst)
  passes(output)
})

test_that("check_object step by step - custom - 2", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "undef <- 'undef'
                 incorr <- 'incorr'
                 ex() %>% check_object('x', undefined_msg = undef) %>% check_equal(incorrect_msg = incorr)"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Undef")

  lst$DC_CODE <- "x <- 4"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorr")

  lst$DC_CODE <- "x <- 5"
  output <- test_it(lst)
  passes(output)
})


test_that("check_object - backwards compatible", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
lst$DC_SCT <- "test_object('x')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define")

  lst$DC_CODE <- "x <- 4"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of")

  lst$DC_CODE <- "x <- 5"
  output <- test_it(lst)
  passes(output)
})



test_that("test_object - eval", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "test_object('x', eval = FALSE)"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "x <- 4"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object - eq_condition = equivalent", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- c(a = 1, b = 2)"
  lst$DC_SCT <- "test_object('x')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "x <- c(a = 1, c = 3)"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "x <- c(a = 1, c = 2)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object - eq_condition = equal", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- c(a = 1, b = 2)"
  lst$DC_SCT <- "test_object('x', eq_condition = 'equal')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "x <- c(a = 1, c = 3)"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "x <- c(a = 1, c = 2)"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "x <- c(a = 1, b = 2)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object - eq_condition = identical", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 3"
  lst$DC_CODE <- "x <- 3 + 4.4e-8"

  lst$DC_SCT <- "test_object('x', eq_condition = 'equivalent')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_object('x', eq_condition = 'equal')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_object('x', eq_condition = 'identical')"
  output <- test_it(lst)
  fails(output)
})

test_that("check_object - custom eq_fun", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- list(a = 1)"
  lst$DC_SCT <- "ex() %>% check_object('x') %>% check_equal(eq_fun = function(x, y) { x$a == y$a })"

  # correct
  exs <- list(
    list(code = "x <- list(a = 1, b = 2)", correct = TRUE),
    list(code = "x <- list(a = 2)", correct = FALSE),
    list(code = "x <- 1", correct = FALSE)
  )

  for (ex in exs) {
    lst$DC_CODE <- ex$code
    output <- test_it(c(lst, DC_CODE = ex$code))
    if (ex$correct) passes(output) else fails(output)
  }
})

test_that("test_object - different classes - 1", {
  lst <- list()
  lst$DC_CODE <- "carriers <- c(1, 2, 3)"
  lst$DC_SOLUTION <- "carriers <- c('a', 'b', 'c')"
  lst$DC_SCT <- "test_object('carriers')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_object - different class - 2", {
  lst <- list()
  lst$DC_SOLUTION <- "number <- which(c('a', 'b', 'c') == 'b')"

  lst$DC_CODE <-"number <- 2"
  lst$DC_SCT <- "test_object('number')"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <-"number <- 1"
  lst$DC_SCT <- "test_object('number')"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <-"number <- which(c('a', 'b', 'c') == 'c')"
  lst$DC_SCT <- "test_object('number')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_object - line numbers - 1", {
  lst <- list()
  lst$DC_CODE <- "a <- 5\n\ny <- 4\n\nz <- 6\n\nz <- 7"
  lst$DC_SOLUTION <- "x <- 4\ny <- 5\nz <- 6"

  # If no assignments in student code, show nothing
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)

  # If single assignment in student code, show line
  lst$DC_SCT <- "test_object('y')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 3, 3)

  # If two assignments in student code, show nothing
  lst$DC_SCT <- "test_object('z')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
})

test_that("test_object - line numbers - 2", {
  lst <- list()
  lst$DC_CODE <- "x <- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst <- list()
  lst$DC_CODE <- "x <- 5\ny <- 6\n7 ->> z"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst <- list()
  lst$DC_CODE <- "5 -> x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst <- list()
  lst$DC_CODE <- "5 -> a\nx <- 6"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 2, 2)

  lst <- list()
  lst$DC_CODE <- "x = 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst <- list()
  lst$DC_PEC <- "x <- c(1, 2, 3)"
  lst$DC_CODE <- "x[x < 2] = 0"
  lst$DC_SOLUTION <- "x <- c(1, 2, 3)"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  line_info(output, 1, 1)

  # if x is used on the wrong side, shouldn't be taken as assignment
  lst <- list()
  lst$DC_CODE <- "x <- 5\ny <- x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
})

test_that("test_object - diff messages", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 'test'\nb <- 1:10\nc <- c(x = 1, y = 2)"
  lst$DC_CODE <- "a <- 1\nb <- 1:20\nc <- c(x = 1, z = 2)"

  lst$DC_SCT <- "test_object('a')"
  output <- test_it(lst)
  fails(output, mess_patt = "It is a number, while it should be a character string.")

  lst$DC_SCT <- "test_object('b')"
  output <- test_it(lst)
  fails(output, mess_patt = "It has length 20, while it should have length 10")

  lst$DC_SCT <- "test_object('c')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_object('c', eq_condition = 'equal')"
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure the attributes")
})

test_that("check_object in combination with test_or", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 2"
  lst$DC_SCT <- "check_or(ex() %>% check_object('a') %>% check_equal(),
                          ex() %>% override_solution_env(a = 2.5) %>% check_object('a') %>% check_equal(),
                          ex() %>% override_solution_env(a = 3) %>% check_object('a') %>% check_equal())"

  lst$DC_CODE <- "a <- 2"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "a <- 2.5"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "a <- 3"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "a <- 4"
  output <- test_it(lst)
  fails(output)
})

test_that("check_object in combination with test_or", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 2"
  lst$DC_SCT <- "check_or(ex() %>% check_object('a') %>% check_equal(),
                          ex() %>% override_solution_env(a = 2.5) %>% check_object('a') %>% check_equal())
                 ex() %>% check_object('a') %>% check_equal()"

  lst$DC_CODE <- "a <- 2"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "a <- 2.5"
  output <- test_it(lst)
  fails(output)
})



test_that("test_object inside MarkdownExercise doesn't show line numbers", {
  lst <- list()
  lst$DC_TYPE <- "MarkdownExercise"
  lst$DC_SOLUTION <- c(my_solution.Rmd = "# This is a test\n```{r}\nx <- 5\n```\n")
  lst$DC_CODE <- c(my_doc.Rmd = "# This is a test\n```{r}\nx <- 5\n```\n")
  lst$DC_SCT <- "test_rmd_group(2, test_object('x'))\nsuccess_msg(\"OK\")"

  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- c(my_doc.Rmd = "# This is a test\n```{r}\nx <- 4\n```\n")
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
})

context("test_column")

test_that("test_column - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_SCT <- "dfstate <- ex() %>% check_object('df')
  dfstate %>% check_column('a') %>% check_equal()
  dfstate %>% check_column('b') %>% check_equal()"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define the variable")

  lst$DC_CODE <- "df <- data.frame(c = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>a</code>")

  lst$DC_CODE <- "df <- data.frame(a = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>a</code> doesn&#39;t seem to be correct")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>b</code>")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('r', 's', 't'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>b</code> doesn&#39;t seem to be correct")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_column - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_SCT <- "dfstate <- ex() %>% check_object('df', undefined_msg = 'undefined')
  dfstate %>% check_column('a', col_missing_msg = 'missinga') %>% check_equal(incorrect_msg = 'incorra')
  dfstate %>% check_column('b', col_missing_msg = 'missingb') %>% check_equal(incorrect_msg = 'incorrb')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Undefined")

  lst$DC_CODE <- "df <- data.frame(c = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Missinga")

  lst$DC_CODE <- "df <- data.frame(a = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Incorra")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Missingb")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('r', 's', 't'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Incorrb")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_column - backwards compatibility", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_SCT <- "test_data_frame('df')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define the variable")

  lst$DC_CODE <- "df <- data.frame(c = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>a</code>")

  lst$DC_CODE <- "df <- data.frame(a = c(4, 5, 6))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>a</code> doesn&#39;t seem to be correct")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain a column <code>b</code>")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('r', 's', 't'))"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The column <code>b</code> doesn&#39;t seem to be correct")

  lst$DC_CODE <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_column - eq_condition", {
  lst <- list()
  lst$DC_SOLUTION <- "df <- data.frame(a = c(1, 2, 3), b = c('x', 'y', 'z'))"
  lst$DC_CODE <- "df <- data.frame(a = c(1 + 4.4e-9, 2, 3), b = c('x', 'y', 'z'), row.names = c('r', 's', 't'))"

  lst$DC_SCT <- "ex() %>% check_object('df') %>% check_column('a') %>% check_equal(eq_condition = 'equivalent')"
  output <- test_it(lst)
  passes(output)

  # Maybe the different row names should cause a fail here... ?
  lst$DC_SCT <- "ex() %>% check_object('df') %>% check_column('a') %>% check_equal(eq_condition = 'equal')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% check_object('df') %>% check_column('a') %>% check_equal(eq_condition = 'identical')"
  output <- test_it(lst)
  fails(output)
})

context("test_element")

test_that("test_element - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "lst <- list(a = 1, b = 2)"
  lst$DC_SCT <- "lststate <- ex() %>% check_object('lst')
                 lststate %>% check_element('a') %>% check_equal()
                 lststate %>% check_element('b') %>% check_equal()"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Did you define the variable")

  lst$DC_CODE <- "lst <- list(c = 3)"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain an element <code>a</code>")

  lst$DC_CODE <- "lst <- list(a = 2)"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The element <code>a</code> doesn&#39;t seem to be correct")

  lst$DC_CODE <- "lst <- data.frame(a = 1)"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "Does it contain an element <code>b</code>")

  lst$DC_CODE <- "lst <- data.frame(a = 1, b = 3)"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable")
  fails(output, mess_patt = "The element <code>b</code> doesn&#39;t seem to be correct")

  lst$DC_CODE <- "lst <- data.frame(a = 1, b = 2)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object fails if ENV set", {
  setup_state(stu_code = 'x <- 5', sol_code = 'x <- 5')
  withr::with_envvar(c(TESTWHAT_V2_ONLY = ''), {
    passes2(test_object('x'))
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '0'), {
    passes2(test_object('x'))
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(test_object('x'), regexp = 'test_object() can no longer be used in SCTs', fixed = TRUE)
  })
})

