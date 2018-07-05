# SETUP -----------------------------------------------------------------------

contents = "
---
title: 'Title'
author: 'nick'
output:
  html_document:
    toc: true
---

```{r global, include=FALSE}
library(ggplot2)
my_df <- mtcars
```

# H1 Header

## H2 header 1

H2 header 1 content

H2 header 2
-----------------------------------------------------------------------

### h3 header 1

```{r echo = FALSE, error = FALSE, label = 'origins'}
ggplot(my_df, aes(x = wt, y = hp)) + geom_point()
```

## Column {data-width=350}

### h3 header 2

```{r label = 'h3h2'}
ggplot(my_df, aes(x = hp, y = wt)) + geom_point()
```

### h3 header 3

```{r label = 'h3h3'}
ggplot(my_df, aes(x = hp, y = mpg)) + geom_point()
```
"

con <- c(my_file.Rmd = contents)
state <- setup_state(stu_code = con,
                     sol_code = con,
                     ex_type = "MarkdownExercise")

# STATE MANIP -----------------------------------------------------------------
context("test_check_rmd - state manipulation")

test_that("check_rmd_file zooms in properly", {
  state <- setup_state(stu_code = con, sol_code = con, ex_type = "MarkdownExercise")
  expect_equal(state$get('student_code'), con)
  expect_equal(state$get('solution_code'), con)
  new_state <- check_rmd(state)
  expect_equal(new_state$get('student_code'), contents)
  expect_equal(new_state$get('solution_code'), contents)
})

test_that("check_header and check_title zoom in properly", {
  state <- setup_state(stu_code = con, sol_code = con, ex_type = "MarkdownExercise")

  new_state <- state %>%
    check_rmd() %>%
    check_header(level = 2, index = 1)
  title <- '## H2 header 1'
  header_con <- '\nH2 header 1 content\n'
  expect_equal(new_state$get('student_title'), title)
  expect_equal(new_state$get('solution_title'), title)
  expect_equal(new_state$get('student_code'), header_con)
  expect_equal(new_state$get('solution_code'), header_con)
  new_state_title <- new_state %>% check_title()
  expect_equal(new_state_title$get('student_code'), title)
  expect_equal(new_state_title$get('solution_code'), title)

  new_state2 <- state %>%
    check_rmd() %>%
    check_header(level = 3, index = 2)
  title <- '### h3 header 2'
  header_con <- "\n```{r label = 'h3h2'}\nggplot(my_df, aes(x = hp, y = wt)) + geom_point()\n```\n"
  expect_equal(new_state2$get('student_title'), title)
  expect_equal(new_state2$get('solution_title'), title)
  expect_equal(new_state2$get('student_code'), header_con)
  expect_equal(new_state2$get('solution_code'), header_con)
  new_state2_title <- new_state2 %>% check_title()
  expect_equal(new_state2_title$get('student_code'), title)
  expect_equal(new_state2_title$get('solution_code'), title)

  # header-in-header chaining works (should give same as new_state2)
  new_state3 <- state %>%
    check_rmd() %>%
    check_header(level = 2, index = 3) %>%
    check_header(level = 3, index = 1)
  expect_equal(new_state3$get('student_title'), title)
  expect_equal(new_state3$get('solution_title'), title)
  expect_equal(new_state3$get('student_code'), header_con)
  expect_equal(new_state3$get('solution_code'), header_con)
  new_state3_title <- new_state3 %>% check_title()
  expect_equal(new_state3_title$get('student_code'), title)
  expect_equal(new_state3_title$get('solution_code'), title)
})

test_that("check_chunk zooms in properly", {
  new_state <- state %>%
    check_rmd() %>%
    check_header(level = 3, index = 2) %>%
    check_chunk(1)
  code <- "ggplot(my_df, aes(x = hp, y = wt)) + geom_point()"
  expect_equal(new_state$get('student_code'), code)
  expect_equal(new_state$get('solution_code'), code)

  new_state2 <- state %>%
    check_rmd() %>%
    check_header(level = 2, index = 3) %>%
    check_header(level = 3, index = 2) %>%
    check_chunk(1)
  code <- "ggplot(my_df, aes(x = hp, y = mpg)) + geom_point()"
  expect_equal(new_state2$get('student_code'), code)
  expect_equal(new_state2$get('solution_code'), code)
})

# STUDENT ERRORS --------------------------------------------------------------

context("test_check_rmd - student errors")

test_that("student didn't specify header", {
  new_state <- state
  new_state$set(student_code = c(my_file.Rmd = ""))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 1),
               regexp = "Have you included one `h2` header",
               class = "sct_failure")

  new_state$set(student_code = c(my_file.Rmd = "\n## My title\n\n"))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 3, index = 1),
               regexp = "Have you included one `h3` header",
               class = "sct_failure")
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 2),
               regexp = "Have you included two `h2` headers",
               class = "sct_failure")
  passes2(new_state %>%
            check_rmd() %>%
            check_header(level = 2, index = 1))
})

test_that("student made mistake in the title", {
  new_state <- state
  new_state$set(student_code = c(my_file.Rmd = "\n\nRow\n------------------\n"))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 1) %>%
                 check_title() %>%
                 check_code("Column", missing_msg = "Wrong."),
               regexp = "Check the first `h2` header. Check the title. Wrong.",
               class = "sct_failure")
  passes2(new_state %>%
            check_rmd() %>%
            check_header(level = 2, index = 1) %>%
            check_title() %>%
            check_code("o"))
})

test_that("student didn't specify chunk", {
  new_state <- state
  new_state$set(student_code = c(my_file.Rmd = ""))
  expect_error(new_state %>% check_rmd() %>% check_chunk(),
               regexp = "Have you included one code chunk",
               class = "sct_failure")

  new_state$set(student_code = c(my_file.Rmd = "## H2 with code\n\n```{r}\nx\n````\n\n## H2 without code\n"))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 2) %>%
                 check_chunk(),
               regexp = "Check the second `h2` header. Have you included one code chunk?",
               class = "sct_failure")
  passes2(new_state %>% check_rmd() %>% check_chunk())
})

test_that("student made mistake in chunk", {
  new_state <- state
  new_state$set(student_code = c(my_file.Rmd = "## H2 without\n\n## H2 with code\n```{r}\nmy_df\n````"))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 2) %>%
                 check_chunk() %>%
                 check_function('ggplot'),
               regexp = "Check the second `h2` header. Have a look at the first code chunk. Have you called `ggplot()`?",
               class = "sct_failure")
  passes2(new_state %>%
           check_rmd() %>%
           check_header(level = 2, index = 2) %>%
           check_chunk() %>%
           check_code('my_df'))
})

test_that("student made mistake in chunk options", {
  new_state <- state
  new_state$set(student_code = c(my_file.Rmd = "## H2 without\n\n## H2 with code\n```{r error = FALSE}\nmy_df\n````"))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 2) %>%
                 check_chunk() %>%
                 check_option('echo'),
               regexp = "Check the second `h2` header. Have a look at the first code chunk. Have you specified the chunk option `echo`?",
               class = "sct_failure")
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_header(level = 2, index = 2) %>%
                 check_chunk() %>%
                 check_option('label') %>%
                 check_equal(),
               regexp = "Check the second `h2` header. Have a look at the first code chunk. The chunk option `label` isn't correct.",
               class = "sct_failure")
  passes2(new_state %>%
            check_rmd() %>%
            check_header(level = 2, index = 2) %>%
            check_chunk() %>%
            check_option('error') %>%
            check_equal())
})

test_that("student made mistake in YAML header", {
  new_state <- state
  
  new_state$set(student_code = c(my_file.Rmd = gsub("\nauthor: 'nick'", "\nincorrect%&@63", contents)))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_yaml() %>%
                 check_option('author'),
               regexp = "Something went wrong when parsing the YAML header. Are you sure you indented everything properly?",
               class = "sct_failure")
  
  new_state$set(student_code = c(my_file.Rmd = gsub("\nauthor: 'nick'", "", contents))) # author missing
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_yaml() %>%
                 check_option('author'),
               regexp = "Check your YAML header. Have you specified the YAML header option `author`?",
               class = "sct_failure")
  
  new_state$set(student_code = c(my_file.Rmd = gsub("\nauthor: 'nick'", "\nauthor: 'sumedh'", contents)))
  expect_error(new_state %>%
                 check_rmd() %>%
                 check_yaml() %>%
                 check_option('author') %>%
                 check_equal(),
               regexp = "Check your YAML header. The option `author` is not correct",
               class = "sct_failure")

  passes2(new_state %>%
                 check_rmd() %>%
                 check_yaml() %>%
                 check_option(c('output', 'html_document', 'toc')) %>%
                 check_equal())
})

# FULL EXAMPLE ----------------------------------------------------------------

context("test_check_rmd - full example")

test_that("full example", {

  passes2(state %>% check_rmd() %>% {
    check_yaml(.) %>% check_option('title') %>% check_equal()
    check_header(., level = 1, index = 1) %>% check_title() %>% check_code("H1 Header")
    check_header(., level = 2, index = 1) %>% {
      check_title(.) %>% check_code('H2 header 1', fixed = TRUE)
      check_code(., 'H2 header 1 content', fixed = TRUE)
    }
    check_header(., level = 2, index = 2) %>% {
      check_title(.) %>% check_code('H2 header 2', fixed = TRUE)
      check_header(., level = 3, index = 1) %>% {
        check_title(.) %>% check_code('h3 header 1', fixed = TRUE)
        check_chunk(.) %>% {
          check_option(., 'label') %>% check_equal()
          check_function(.,'ggplot') %>% check_arg('data') %>% check_equal()
        }
      }
    }
  })
})
