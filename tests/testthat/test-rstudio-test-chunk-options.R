context("test_chunk_options")
source("helpers.R")

test_that("test_chunk_options works", {
  # source("tests/testthat/helpers.R")
  lst <- list()
  lst$DC_TYPE <- "MarkdownExercise"
  lst$DC_ACTIVE_TAB <- "my_doc.Rmd"
  lst$DC_FORMAT <- "HTML"
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE}\n    plot(cars)\n    ```\n      \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, echo=FALSE}\n    str(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, message = FALSE}\n    plot(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, message = FALSE}\n    str(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  passes(output)
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"message\")))"
  output <- test_it(lst)
  passes(output)                
                  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE, testing = \"amaitestenissaai\"}\n    plot(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, echo=FALSE, testing = \"amaitestenissaai\"}\n    str(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\", \"testing\")))"
  output <- test_it(lst)
  passes(output)                
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE, testing = TRUE}\n    plot(cars)\n    ```\n    \n    Wowww.\n  \n    ```{r, veelteveel = \"oeps\", echo=FALSE, eval = FALSE}\n    nikske(cars)\n    ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r echo = TRUE}\n    dim(cars)\n    ```\n\n    You can also embed plots, for falafel:\n    \n    ```{r     echo=TRUE, testing = TRUE ,echo = FALSE}\n    str(cars)\n    ```\n    \n    Wowww.\n  \n    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\"}\n    zever(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  passes(output)                
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\", \"testing\")))"
  output <- test_it(lst)
  passes(output)                
  lst$DC_SCT <- "test_rmd_group(6, test_chunk_options(options = c(\"echo\", \"eval\", \"veelteveel\")))"
  output <- test_it(lst)
  passes(output)                
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE, testing = FALSE, extra = 123}\n    plot(cars)\n    ```\n    \n    Wowww.\n    \n    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\", this_isnt_relevant = FALSE}\n    nikske(cars)\n    ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r     echo=FALSE, testing = TRUE}\n    str(cars)\n    ```\n    \n    Wowww.\n    \n    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\", grr = \"not_important\"}\n    zever(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  passes(output)                
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  passes(output)                
  lst$DC_SCT <- "test_rmd_group(6, test_chunk_options(options = c(\"echo\", \"eval\", \"veelteveel\")))"
  output <- test_it(lst)
  passes(output)                
                  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=TRUE}\n    plot(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r, in_solution = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, echo=FALSE}\n    str(cars)\n    ```\n    \n    Wowww."))
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  fails(output, mess_patt = "In code chunk 2 of your submission, make sure to correctly")
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"in_solution\")))"
  output <- test_it(lst)
  fails(output, mess_patt = "Code chunk 1 of your submission should contain")
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"in_solution\"), not_called_msg = \"Not_called_at_all\"))"
  output <- test_it(lst)
  fails(output, mess_patt = "Not_called_at_all")
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\"), incorrect_msg = \"Incorrect, dude\"))"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect, dude")
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = FALSE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, message = TRUE}\n    plot(cars)\n    ```\n    \n    Wowww.")) 
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r        echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, message = FALSE}\n    str(cars)\n    ```\n    \n    Wowww.")) 
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"message\")))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE, testing = \"amaitestenissaai\"}\n    plot(cars)\n    ```\n    \n    Wowww.")) 
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r, echo=TRUE, testing = \"hehehe\"}\n    str(cars)\n    ```\n    \n    Wowww.")) 
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\", \"testing\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = \"echo\"))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = FALSE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE, testing = FALSE, echo  = FALSE}\n    plot(cars)\n    ```\n    \n    Wowww.\n    \n    ```{r, veelteveel = \"oeps\", echo=FALSE, eval = FALSE, eval = TRUE}\n    nikske(cars)\n    ```")) 
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r     echo=TRUE, testing = TRUE ,echo = FALSE}\n    str(cars)\n    ```\n    \n    Wowww.\n    \n    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\"}\n    zever(cars)\n    ```")) 
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"echo\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\", \"testing\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_group(6, test_chunk_options(options = c(\"echo\", \"eval\", \"veelteveel\")))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r, echo = TRUE,     not_allowed = FALSE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for example:\n    \n    ```{r, echo=FALSE, testing = FALSE}\n    plot(cars)\n    ```\n    \n    Wowww.\n    \n    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\", this_isnt_relevant = FALSE}\n    nikske(cars)\n    ```")) 
  lst$DC_SOLUTION <- rjson::toJSON(list(my_sol.Rmd = "\n    ---\n    title: \"Testing\"\n    author: \"Tester\"\n    date: \"January 1, 2015\"\n    output: html_document\n    ---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r echo = TRUE}\n    dim(cars)\n    ```\n    \n    You can also embed plots, for falafel:\n    \n    ```{r     echo=FALSE, testing = TRUE}\n    str(cars)\n    ```\n    \n    Wowww.\n    \n    ```{r, echo=FALSE, eval = FALSE, veelteveel = \"oeps\"}\n    zever(cars)\n    ```")) 
  lst$DC_SCT <- "test_rmd_group(2, test_chunk_options(options = c(\"echo\"), allow_extra = FALSE))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_group(4, test_chunk_options(options = c(\"echo\"), allow_extra = FALSE))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_group(6, test_chunk_options(options = c(\"echo\", \"eval\", \"veelteveel\"), allow_extra = FALSE))"
  output <- test_it(lst)
  fails(output, mess_patt = "Do not define any other options!")
  
})
