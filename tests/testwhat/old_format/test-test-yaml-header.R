source("testing-framework.R")

# Scenarios for test-yaml-header

# Scenario 1: check if one option is recognized correctly
test_rmd_scenario(
  name = "test_one_option",
  student = "
---
title: \"Testing\"
---
    
    This
    is
    a
    test
    
    ```{r}
    dim(cars)
    ```",
  solution = "
---
title: \"Testing\"
---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r}
    str(cars)
    ```",
  msg = "test if one option is correctly recognized",
  passes = function() {
    test_yaml_header(options = c("title"))
  }
)

# Scenario 2: check if two options are recognized correctly
test_rmd_scenario(
  name = "test_two_options",
  student = "
---
title: \"Testing\"
output: html_document
---
    
    This
    is
    a
    test
    
    ```{r}
    dim(cars)
    ```",
  solution = "
---
title: \"Testing\"
output: html_document
---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r}
    str(cars)
    ```",
  msg = "test if two options are correctly recognized",
  passes = function() {
    test_yaml_header(options = c("title", "output"))
  }
)

# Scenario 3: check if three options are recognized correctly
test_rmd_scenario(
  name = "test_three_options",
  student = "
---
title: \"Testing\"
author: \"Tester\"
output: html_document
---
    
    This
    is
    a
    test
    
    ```{r}
    dim(cars)
    ```",
  solution = "
---
title: \"Testing\"
author: \"Tester\"
output: html_document
---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r}
    str(cars)
    ```",
  msg = "test if three options are correctly recognized",
  passes = function() {
    test_yaml_header(options = c("title", "output", "author"))
  }
)

# Scenario 4: check if complex options are recognized correctly (with same)
test_rmd_scenario(
  name = "test_complex_options",
  student = "
---
title: \"Testing\"
author: \"Tester\"
output: html_document
input: html_document
this:
  is:
    a:
      complex:
        test: true
      simple:
        test: false
    irrelevant: true
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
input: html_document
title: \"Testing\"
author: \"Tester\"
output: html_document
this:
  is:
    a:
      complex:
        test: true
      simple:
        test: false
---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r}
    str(cars)
    ```",
  msg = "test if complex options are correctly recognized",
  passes = function() {
    test_yaml_header(options = c("title", "input", "output", "author"))
    test_yaml_header(options = "this.is.a.complex.test")
    test_yaml_header(options = "this.is.a.simple.test")
  }
)

# Scenario 5: check if allow_extra works correctly
test_rmd_scenario(
  name = "test_allow_extra",
  student = "
---
title: \"Testing\"
author: \"Tester\"
output: html_document
input: html_document
extra: nevermind
other: something
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
input: html_document
title: \"Testing\"
author: \"Tester\"
output: html_document
other: something_else
---
  
  This
  is
  a
  test
  but
  doesn't 
    matter
    
    ```{r}
    str(cars)
    ```",
  msg = "test if allow_extra works correctly",
  passes = function() {
    test_yaml_header(options = c("title", "input", "output", "author"))
  }
)

# Scenario 6: check if check_equality works correctly
test_rmd_scenario(
  name = "test_check_equality",
  student = "
---
title: \"Testing\"
author: \"Testerke 2\"
output: html_document
input: html_document
extra: nevermind
other: something
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
input: html_document
title: \"Testing\"
author: \"Tester\"
output: html_document
other: something_else
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if check_equality works correctly",
  passes = function() {
    test_yaml_header(options = c("title", "input", "output"))
    test_yaml_header(options = "author", check_equality = FALSE)
  }
)

# Scenario 7: check if one option fails correctly
test_rmd_scenario(
  name = "test_one_option_fails",
  student = "
---
title: \"Wrong\"
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"Testing\"
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if one option fails correctly",
  passes = function() {
    expect_fail(test_yaml_header(options = c("title")))
  }
)

# Scenario 8: check if two options fail correctly
test_rmd_scenario(
  name = "test_two_options_fail",
  student = "
---
title: \"Wrong\"
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"Testing\"
output: html_document
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if two options fail correctly",
  passes = function() {
    expect_fail(test_yaml_header(options = c("title", "output")))
    expect_fail(test_yaml_header(options = "title"))
    expect_fail(test_yaml_header(options = "output"))
  }
)

# Scenario 9: check if three options fail correctly
test_rmd_scenario(
  name = "test_three_options_fail",
  student = "
---
title: \"Wrong\"
author: \"Not right\"
output: pdf_document
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"Testing\"
author: \"Tester\"
output: html_document
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if three options fail correctly",
  passes = function() {
    expect_fail(test_yaml_header(options = c("title", "output", "author")))
    expect_fail(test_yaml_header(options = c("title", "output")))
    expect_fail(test_yaml_header(options = "title"))
    expect_fail(test_yaml_header(options = "output"))
    expect_fail(test_yaml_header(options = "author"))
  }
)

# Scenario 10: check if complex options fail correctly
test_rmd_scenario(
  name = "test_complex_options_fail",
  student = "
---
title: \"Testing\"
author: \"Tester\"
output: 
  html_document:
    test: true
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"Testing\"
author: \"Tester\"
output: 
  html_document:
    test: false
  pdf_document:
    test: true
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if complex options fail correctly",
  passes = function() {
    expect_fail(
      test_yaml_header(options = c("title", "output.html_document.test", "author", "output.pdf_document.test"))
    )
    expect_fail(
      test_yaml_header(options = c("title", "author", "output.pdf_document.test"))
    )
    expect_fail(
      test_yaml_header(options = c("output.pdf_document.test"))
    )
  }
)

# Scenario 11: check if allow_extra fail correctly
test_rmd_scenario(
  name = "test_allow_extra_fail",
  student = "
---
title: \"Testing\"
author: \"Tester\"
output: 
  html_document:
    test: true
  extra: \"NOOO\"
  pdf_document:
    test: true
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"Testing\"
author: \"Tester\"
output: 
  html_document:
    test: false
  pdf_document:
    test: true
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if allow_extra fails correctly",
  passes = function() {
    expect_fail(
      test_yaml_header(options = c("title", "output.html_document.test", "author"), allow_extra = FALSE)
    )
    expect_fail(
      test_yaml_header(options = c("title", "author", "output.pdf_document.test"), allow_extra = FALSE)
    )
    expect_fail(
      test_yaml_header(options = c("output.pdf_document.test"), allow_extra = FALSE)
    )
  }
)

# Scenario 12: check if check_equality fail correctly
test_rmd_scenario(
  name = "test_check_equality_fail",
  student = "
---
title: \"Testing\"
output: 
  html_document:
    test: true
    extra: \"NOOO\"
  pdf_document:
    test: true
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"Tersting\"
output: 
  html_document:
    test: true
    extra: \"NOOO\"
  pdf_document:
    test: true
author: \"Tester\"
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if check_equality fails correctly",
  passes = function() {
    expect_fail(
      test_yaml_header(options = c("title", "output.html_document.test", "author"), check_equality = FALSE)
    )
    expect_fail(
      test_yaml_header(options = c("author"), check_equality = FALSE)
    )
  }
)

# Scenario 13: check if yaml error fail correctly
test_rmd_scenario(
  name = "test_yaml_structure_error",
  student = "
---
title: \"Testing\"
title: \"OOPS\"
---
  
  This
  is
  a
  test
  
  ```{r}
  dim(cars)
  ```",
  solution = "
---
title: \"OOPS\"
---
  
  This
  is
  a
  test
  but
  doesn't 
  matter
  
  ```{r}
  str(cars)
  ```",
  msg = "test if check_equality fails correctly",
  passes = function() {
    expect_fail(
      test_yaml_header(options = c("title"))
    )
  }
)
