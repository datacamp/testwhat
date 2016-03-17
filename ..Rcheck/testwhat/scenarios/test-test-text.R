scen <- list(
  list(
    type = "MarkdownExercise", 
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
    
    testing **good**
    
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
    
    testing **good**
    
    ```{r}
    dim(cars)
    ```
    
    You can also embed plots, for example:
    
    ```{r, echo=FALSE}
    plot(cars)
    ```
    
    Wowww.",
    pass = list(
      test_text_passes_once = list(
        long = "test passes if text is availble in given inline group",
        sct = "test_rmd_group(1, test_text(\"test\"))"
        ),
      test_text_passes_twice = list(
        long = "test passes if text is availble in given inline group twice",
        sct = "test_rmd_group(1, test_text(\"test\", freq = 2))"
      ),
      test_text_passes_with_format = list(
        long = "test passes if text is availble in given inline group in the given format",
        sct = "test_rmd_group(1, test_text(\"good\", format = \"bold\"))"
      )
    ),
    fail =  list(
      test_text_fails_once = list(
        long = "test fails if text is not availble in given inline group",
        sct = "test_rmd_group(1, test_text(\"rest\", not_called_msg = \"not calllled\"))",
        message = "not calllled"
      ),
      test_text_fails_twice = list(
        long = "test fails if text is not availble in given inline group twice",
        sct = "test_rmd_group(1, test_text(\"good\", freq = 2, not_called_msg = \"not called twice\"))",
        message = "not called twice"
      ),
      test_text_passes_with_format = list(
        long = "test fails if text is not availble in given inline group in the given format",
        sct = "test_rmd_group(1, test_text(\"good\", format = \"parentheses\", incorrect_msg = \"that is not correct\"))",
        message = "that is not correct"
      )
    )
  )
)

