context("test_rmd_group")
source("helpers.R")

test_that("test_rmd_group works", {
  lst <- list()
  lst$DC_TYPE <- "MarkdownExercise"
  lst$DC_ACTIVE_TAB <- "my_doc.Rmd"
  lst$DC_FORMAT <- "HTML"
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE}\n    plot(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, echo=FALSE}\n    str(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SCT <- "test_rmd_group(1, NULL)"
  output <- test_it(lst)
  passes(output)
    
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, message = FALSE}\n    plot(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, message = FALSE}\n    str(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_group(1, NULL)"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure the structure of your document is OK.") 
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, message = FALSE}\n    plot(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, message = FALSE}\n    str(cars)\n    ```\n    \n    ```{r}\n    mean(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_group(1, NULL)"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure you have the correct amount of inline")
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, message = FALSE}\n    plot(cars)\n    ```\n    \n    Wowww.\n\n    ```{r}\n    mean(cars)\n    ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, message = FALSE}\n    str(cars)\n    ```\n    \n    ```{r}\n    mean(cars)\n    ```\n    \n    Woowwww."))
  lst$DC_SCT <- "test_rmd_group(1, NULL)"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure the overall code structure of your document is OK.")
})