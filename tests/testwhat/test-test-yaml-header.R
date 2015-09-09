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

# Scenario 4: check if four options are recognized correctly (with same)
test_rmd_scenario(
  name = "test_four_options",
  student = "
---
title: \"Testing\"
author: \"Tester\"
output: html_document
input: html_document
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
    test_yaml_header(options = c("title", "input", "output", "author"))
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

# Scenario 5: check if one option fails correctly
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

# Scenario 6: check if two options fail correctly
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

# Scenario 4: check if four options are recognized correctly (with same)
test_rmd_scenario(
  name = "test_four_options",
  student = "
  ---
  title: \"Testing\"
  author: \"Tester\"
  output: html_document
  input: html_document
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
    test_yaml_header(options = c("title", "input", "output", "author"))
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

