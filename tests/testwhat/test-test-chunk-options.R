source("testing-framework.R")

# Scenarios for test-chunk-options

# Scenario 1: check if one option is recognized correctly
test_rmd_scenario(
  name = "test_one_option",
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
  msg = "test if one option is correctly recognized",
  passes = function() {
      test_rmd_group(4, {
        test_chunk_options(options = c("echo"))
      })
  }
)

# Scenario 2: check if two options are correctly recognized in different chunks
test_rmd_scenario(
  name = "test_two_options_two_chunks",
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
    
    Wowww.",
  msg = "test if two options in seperate chunks is correctly recognized",
  passes = function() {
    test_rmd_group(2, {
      test_chunk_options(options = c("echo"))
    })
    test_rmd_group(4, {
      test_chunk_options(options = c("message"))
    })
  }
)

# Scenario 3: check if two options are correctly recognized in the same chunk
test_rmd_scenario(
  name = "test_two_options_one_chunk",
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
    
    ```{r, echo=FALSE, testing = \"amaitestenissaai\"}
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
    
    ```{r, echo=FALSE, testing = \"amaitestenissaai\"}
    str(cars)
    ```
    
    Wowww.",
  msg = "test if two options in seperate chunks is correctly recognized",
  passes = function() {
    test_rmd_group(4, {
      test_chunk_options(options = c("echo", "testing"))
    })
  }
)

# Scenario 4: Check even more option settings
test_rmd_scenario(
  name = "test_multiple_options_multiple_chunks",
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
    
    ```{r, echo=FALSE, testing = TRUE}
    plot(cars)
    ```
    
    Wowww.
  
    ```{r, veelteveel = \"oeps\", echo=FALSE, eval = FALSE}
    nikske(cars)
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
    
    ```{r echo = TRUE}
    dim(cars)
    ```

    You can also embed plots, for falafel:
    
    ```{r     echo=TRUE, testing = TRUE ,echo = FALSE}
    str(cars)
    ```
    
    Wowww.
  
    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\"}
    zever(cars)
    ```",
  msg = "test if multiple options in multiple chunks are recognized",
  passes = function() {
    test_rmd_group(2, {
      test_chunk_options(options = c("echo"))
    })
    test_rmd_group(4, {
      test_chunk_options(options = c("echo", "testing"))
    })
    test_rmd_group(6, {
      test_chunk_options(options = c("echo", "eval", "veelteveel"))
    })
  }
)

# Scenario 5: Check even more option settings with allow_extra
test_rmd_scenario(
  name = "test_allow_extra",
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
    
    ```{r, echo=FALSE, testing = FALSE, extra = 123}
    plot(cars)
    ```
    
    Wowww.
    
    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\", this_isnt_relevant = FALSE}
    nikske(cars)
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
    
    ```{r echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r     echo=FALSE, testing = TRUE}
    str(cars)
    ```
    
    Wowww.
    
    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\", grr = \"not_important\"}
    zever(cars)
    ```",
  msg = "test if multiple options in multiple chunks are recognized with allow_extra",
  passes = function() {
    test_rmd_group(2, {
      test_chunk_options(options = c("echo"))
    })
    test_rmd_group(4, {
      test_chunk_options(options = c("echo"))
    })
    test_rmd_group(6, {
      test_chunk_options(options = c("echo", "eval", "veelteveel"))
    })
  }
)

# Scenario 6: check if one option fails correctly
test_rmd_scenario(
  name = list(
    "test_one_option_fail",
    "test_not_called_msg",
    "test_incorrect_msg"),
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
    
    ```{r, echo=TRUE}
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
    
    ```{r, in_solution = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, echo=FALSE}
    str(cars)
    ```
    
    Wowww.",
  msg = list(
    "test if one option fails correctly",
    "test if not called msg is displayed after fail",
    "test if incorrect msg is displayed after fail"),
  passes = list(
    function() {
      expect_fail(
        test_rmd_group(4, {
          test_chunk_options(options = c("echo"))
        }),
        "In code chunk 2 of your submission, make sure to correctly"
      )
      expect_fail(
        test_rmd_group(2, {
          test_chunk_options(options = c("in_solution"))
        }),
        "Code chunk 1 of your submission should contain"
      )
    },
    function() {
      expect_fail(
        test_rmd_group(2, {
          test_chunk_options(options = c("in_solution"), not_called_msg = "Not_called_at_all")
        }),
        "Not_called_at_all"
      )
    },
    function() {
      expect_fail(
        test_rmd_group(4, {
          test_chunk_options(options = c("echo"), incorrect_msg = "Incorrect, dude!")
        }),
        "Incorrect, dude!"
      )
    }
  )
)

# Scenario 7: check if two options fail correctly in different chunks
test_rmd_scenario(
  name = "test_two_options_fail_two_chunks",
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
    
    ```{r, echo = FALSE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, message = TRUE}
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
    
    ```{r        echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r, message = FALSE}
    str(cars)
    ```
    
    Wowww.",
  msg = "test if two options in seperate chunks fail correctly",
  passes = function() {
    expect_fail(
      test_rmd_group(2, {
        test_chunk_options(options = c("echo"))
      })
    )
    expect_fail(
      test_rmd_group(4, {
        test_chunk_options(options = c("message"))
      })
    )
  }
)

# Scenario 8: check if two options are fail correctly in the same chunk
test_rmd_scenario(
  name = "test_two_options_fail_one_chunk",
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
    
    ```{r, echo=FALSE, testing = \"amaitestenissaai\"}
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
    
    ```{r, echo=TRUE, testing = \"hehehe\"}
    str(cars)
    ```
    
    Wowww.",
  msg = "test if two options in seperate chunks fail correctly",
  passes = function() {
    expect_fail(
      test_rmd_group(4, {
        test_chunk_options(options = c("echo", "testing"))
      })
    )
    expect_fail(
      test_rmd_group(4, {
        test_chunk_options(options = "echo")
      })
    )
  }
)

# Scenario 9: Check even more fail option settings
test_rmd_scenario(
  name = "test_multiple_options_fail_multiple_chunks",
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
    
    ```{r, echo = FALSE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, echo=FALSE, testing = FALSE, echo  = FALSE}
    plot(cars)
    ```
    
    Wowww.
    
    ```{r, veelteveel = \"oeps\", echo=FALSE, eval = FALSE, eval = TRUE}
    nikske(cars)
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
    
    ```{r echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r     echo=TRUE, testing = TRUE ,echo = FALSE}
    str(cars)
    ```
    
    Wowww.
    
    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\"}
    zever(cars)
    ```",
  msg = "test if multiple options in multiple chunks fail correctly",
  passes = function() {
    expect_fail(
      test_rmd_group(2, {
        test_chunk_options(options = c("echo"))
      })
    )
    expect_fail(
      test_rmd_group(4, {
        test_chunk_options(options = c("echo", "testing"))
      })
    )
    expect_fail(
      test_rmd_group(6, {
        test_chunk_options(options = c("echo", "eval", "veelteveel"))
      })
    )
  }
)

# Scenario 10: Check even more option fail settings with allow_extra
test_rmd_scenario(
  name = "test_allow_extra_fails",
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
    
    ```{r, echo = TRUE,     not_allowed = FALSE}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, echo=FALSE, testing = FALSE}
    plot(cars)
    ```
    
    Wowww.
    
    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\", this_isnt_relevant = FALSE}
    nikske(cars)
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
    
    ```{r echo = TRUE}
    dim(cars)
    ```
    
    You can also embed plots, for falafel:
    
    ```{r     echo=FALSE, testing = TRUE}
    str(cars)
    ```
    
    Wowww.
    
    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\"}
    zever(cars)
    ```",
  msg = "test if multiple options in multiple chunks fail correctly with allow_extra",
  passes = function() {
    expect_fail(
      test_rmd_group(2, {
        test_chunk_options(options = c("echo"), allow_extra = FALSE)
      })
    )
    expect_fail(
      test_rmd_group(4, {
        test_chunk_options(options = c("echo"), allow_extra = FALSE)
      })
    )
    expect_fail(
      test_rmd_group(6, {
        test_chunk_options(options = c("echo", "eval", "veelteveel"), allow_extra = FALSE)
      }),
      "Do not define any other options!"
    )
  }
)