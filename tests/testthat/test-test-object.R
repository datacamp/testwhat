context("test_obj")

test_that("test_obj step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "ex() %>% test_obj('x') %>% test_equal()"

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

test_that("test_obj step by step - custom - 1", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "ex() %>% test_obj('x', undefined_msg = 'undef') %>% test_equal(incorrect_msg = 'incorr')"

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

test_that("test_obj step by step - custom - 2", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_SCT <- "undef <- 'undef'
                 incorr <- 'incorr'
                 ex() %>% test_obj('x', undefined_msg = undef) %>% test_equal(incorrect_msg = incorr)"

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


test_that("test_obj - backwards compatible", {
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

test_that("test_object - different classes - 1", {
  lst <- list()
  lst$DC_PEC <- "library(hflights); library(dplyr); hflights <- tbl_df(hflights); "
  lst$DC_CODE <- "carriers <- hflights$TaxiIn"
  lst$DC_SOLUTION <- "carriers <- hflights$UniqueCarrier"
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
  lst$DC_CODE <- "x <<- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst <- list()
  lst$DC_CODE <- "5 ->> x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)

  lst <- list()
  lst$DC_CODE <- "5 ->> a\nx <- 6"
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


# TODO activate test below

# test_that("test_object inside MarkdownExercise doesn't show line numbers", {
#   lst <- list()
#   lst$DC_TYPE <- "MarkdownExercise"
#   lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "# This is a test\n```{r}\nx <- 5\n```\n"))
#   lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "# This is a test\n```{r}\nx <- 5\n```\n"))
#   lst$DC_SCT <- "test_rmd_group(2, test_object('x'))\nsuccess_msg(\"OK\")"
#   lst$DC_FORMAT <- "PDF"
#   lst$DC_ACTIVE_TAB <- "my_doc.Rmd"
#
#   output <- test_it(lst)
#   passes(output)
#
#   lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "# This is a test\n```{r}\nx <- 4\n```\n"))
#   output <- test_it(lst)
#   fails(output)
#   line_info(output, NULL, NULL)
# })
