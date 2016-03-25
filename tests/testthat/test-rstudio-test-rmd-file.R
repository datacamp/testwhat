context("test_rmd_file")
source("helpers.R")

test_that("test_rmd_file works", {
  lst <- list()
  lst$DC_TYPE <- "MarkdownExercise"
  lst$DC_ACTIVE_TAB <- "my_doc.Rmd"
  lst$DC_FORMAT <- "HTML"
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\n---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\n---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    str(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\")))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\noutput: html_document\n---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\noutput: html_document\n---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    str(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output\")))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\n---\n    \n    This\n    is\n    a\n    test\n    \n    ```{r}\n    dim(cars)\n    ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\n---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    str(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output\", \"author\")))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\ninput: html_document\nthis:\n  is:\n    a:\n      complex:\n        test: true\n      simple:\n        test: false\n    irrelevant: true\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ninput: html_document\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\nthis:\n  is:\n    a:\n      complex:\n        test: true\n      simple:\n        test: false\n---\n    \n    This\n    is\n    a\n    test\n    but\n    doesn't \n    matter\n    \n    ```{r}\n    str(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"input\", \"output\", \"author\")))"
  output <- test_it(lst)
  passes(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"this.is.a.complex.test\"))"
  output <- test_it(lst)
  passes(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"this.is.a.simple.test\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\ninput: html_document\nextra: nevermind\nother: something\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ninput: html_document\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\nother: something_else\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n    matter\n    \n    ```{r}\n    str(cars)\n    ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"input\", \"output\", \"author\")))"
  output <- test_it(lst)
  passes(output)
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Testerke 2\"\noutput: html_document\ninput: html_document\nextra: nevermind\nother: something\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ninput: html_document\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\nother: something_else\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"input\", \"output\")))"
  output <- test_it(lst)
  passes(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"author\", check_equality = FALSE))"
  output <- test_it(lst)
  passes(output)
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Wrong\"\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\")))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Wrong\"\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\noutput: html_document\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"title\"))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"output\"))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Wrong\"\nauthor: \"Not right\"\noutput: pdf_document\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: html_document\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output\", \"author\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"title\"))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"output\"))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = \"author\"))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: \n  html_document:\n    test: true\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: \n  html_document:\n    test: false\n  pdf_document:\n    test: true\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output.html_document.test\", \"author\", \"output.pdf_document.test\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"author\", \"output.pdf_document.test\")))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"output.pdf_document.test\")))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: \n  html_document:\n    test: true\n  extra: \"NOOO\"\n  pdf_document:\n    test: true\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Testing\"\nauthor: \"Tester\"\noutput: \n  html_document:\n    test: false\n  pdf_document:\n    test: true\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output.html_document.test\", \"author\"), allow_extra = FALSE))"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_yaml_header(options = c(\"title\", \"author\", \"output.pdf_document.test\"), allow_extra = FALSE)"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"output.pdf_document.test\"), allow_extra = FALSE))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\noutput: \n  html_document:\n    test: true\n    extra: \"NOOO\"\n  pdf_document:\n    test: true\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"Tersting\"\noutput: \n  html_document:\n    test: true\n    extra: \"NOOO\"\n  pdf_document:\n    test: true\nauthor: \"Tester\"\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\", \"output.html_document.test\", \"author\"), check_equality = FALSE))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"author\"), check_equality = FALSE))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "\n---\ntitle: \"Testing\"\ntitle: \"OOPS\"\n---\n  \n  This\n  is\n  a\n  test\n  \n  ```{r}\n  dim(cars)\n  ```"))
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "\n---\ntitle: \"OOPS\"\n---\n  \n  This\n  is\n  a\n  test\n  but\n  doesn't \n  matter\n  \n  ```{r}\n  str(cars)\n  ```"))
  lst$DC_SCT <- "test_rmd_file(test_yaml_header(options = c(\"title\")))"
  output <- test_it(lst)
  fails(output)
  
})
  
