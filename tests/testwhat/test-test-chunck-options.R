source("testing-framework.R")

# Scenarios for test-chunk-options

# Scenario 1: just check for the use of function
test_rmd_scenario(
  name = 'test_one_option',
  student = '
  ---
  title: "Testing"
  author: "Tester"
  date: "January 1, 2015"
  output: html_document
  ---
  
  This
  is
  a
  test
  
  ```{r}
  summary(cars)
  ```
  
  You can also embed plots, for example:
  
  ```{r echo=FALSE}
  plot(cars)
  ```
  
  Wowww.',
  solution = '
  ---
  title: "Testing"
  author: "Tester"
  date: "January 1, 2015"
  output: html_document
  ---
  
  This
  is
  a
  test
  but
  doesn\'t 
  matter
  
  ```{r}
  dim(cars)
  ```
  
  You can also embed plots, for falafel:
  
  ```{r, echo=FALSE}
  str(cars)
  ```
  
  Wowww.',
  msg = 'test if one option is correctly recognized',
  passes = function() {
      test_rmd_group(4, {
        test_chunk_options(options = c("echo"))
      })
  }
)
