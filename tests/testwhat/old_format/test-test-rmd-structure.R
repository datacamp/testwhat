source("testing-framework.R")

# Scenarios for testing the rmd structure

# Scenario 1: test correct structure passes
test_rmd_scenario(
  name = "test_correct_structure",
  student = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    
    ```{r}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, echo=FALSE}
    plot(cars)
    ```
    
    Wowww.",
  solution = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
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
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, echo=FALSE}
    str(cars)
    ```
    
    Wowww.",
  msg = "test if chunks are correctly structured",
  passes = function() {
    test_rmd_group(1, NULL)
  }
)

# Scenario 2: test number of blocks incorrect
test_rmd_scenario(
  name = "test_incorrect_number_of_blocks",
  student = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, message = FALSE}
    plot(cars)
    ```
    
    Wowww.",
  solution = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, message = FALSE}
    str(cars)
    ```",
  msg = "test if incorrect number of blocks fails",
  passes = function() {
    expect_fail(
      test_rmd_group(1, NULL),
      "Make sure the structure of your document is OK."
    )
  }
)

# Scenario 3: test number of inline incorrect
test_rmd_scenario(
  name = "test_incorrect_number_of_inline",
  student = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, message = FALSE}
    plot(cars)
    ```
    
    Wowww.",
  solution = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, message = FALSE}
    str(cars)
    ```
    
    ```{r}
    mean(cars)
    ```",
  msg = "test if incorrect number of inlines fails",
  passes = function() {
    expect_fail(
      test_rmd_group(1, NULL),
      "Make sure you have the correct amount of inline"
    )
  }
)

# Scenario 3: test number of code incorrect
test_rmd_scenario(
  name = "test_incorrect_number_of_code",
  student = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, message = FALSE}
    plot(cars)
    ```
    
    Wowww.",
  solution = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, message = FALSE}
    str(cars)
    ```
    
    ```{r}
    mean(cars)
    ```",
  msg = "test if incorrect number of code fails",
  passes = function() {
    expect_fail(
      test_rmd_group(1, NULL),
      "Make sure you have the correct amount of code blocks"
    )
  }
)

# Scenario 4: test full structure
test_rmd_scenario(
  name = "test_full_structure",
  student = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, message = FALSE}
    plot(cars)
    ```
    
    Wowww.

    ```{r}
    mean(cars)
    ```",
  solution = "
    ---
    title: \"Testing\"
    author: \"Tester\"
    date: \"January 1, 2015\"
    output: html_document
    ---
    
    This
    is
    a
    test
    but
    doesn't 
    matter
    
    ```{r, echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, message = FALSE}
    str(cars)
    ```
    
    ```{r}
    mean(cars)
    ```
    
    Woowwww.",
  msg = "test overall structure",
  passes = function() {
    expect_fail(
      test_rmd_group(1, NULL),
      "Make sure the overall code structure of your document is OK."
    )
  }
)