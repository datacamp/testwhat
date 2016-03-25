context("test_text")
source("helpers.R")

test_that("test_text works", {
  lst <- list()
  lst$DC_TYPE <- "MarkdownExercise"
  lst$DC_ACTIVE_TAB <- "my_doc.Rmd"
  lst$DC_FORMAT <- "HTML"
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "
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
    
    Wowww.")) 
  
    lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "
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
    
    Wowww."))
  
  lst$DC_SCT <- "test_rmd_group(1, test_text(\"test\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_rmd_group(1, test_text(\"test\", freq = 2))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_rmd_group(1, test_text(\"good\", format = \"bold\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_rmd_group(1, test_text(\"rest\", not_called_msg = \"not calllled\"))"
  output <- test_it(lst)
  fails(output, mess_patt = "not calllled")
  
  lst$DC_SCT <- "test_rmd_group(1, test_text(\"good\", freq = 2, not_called_msg = \"not called twice\"))"
  output <- test_it(lst)
  fails(output, mess_patt = "not called twice")
  
  lst$DC_SCT <- "test_rmd_group(1, test_text(\"good\", format = \"parentheses\", incorrect_msg = \"that is not correct\"))"
  output <- test_it(lst)
  fails(output, mess_patt = "that is not correct")
})

