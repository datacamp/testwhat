context("test_object")
source("helpers.R")

test_that("test_object works for simple objects", {
  lst <- list()
  lst$DC_CODE <- "\n    var.equiv <- 3\n    var.not_equiv <- 4"
  lst$DC_SOLUTION <- "\n    var.equiv <- 3\n    var.not_equiv <- 3\n    var.not_here <- 2"
  
  lst$DC_SCT <- "test_object()"
  output <- test_it(lst)
  error(output, mess_patt = "missing, with no default")
  
  lst$DC_SCT <- "test_object(\"var.not_equiv\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var.not_here\", undefined_msg = \"This is the undefined message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the undefined message")
  
  lst$DC_SCT <- "test_object(\"var.not_equiv\", incorrect_msg = \"This is the incorrect message\")"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the incorrect message")
  
  lst$DC_SCT <- "test_object(\"var.equiv\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object resilient to different classes", {
  lst <- list()
  lst$DC_PEC <- "load(url(\"http://s3.amazonaws.com/assets.datacamp.com/course/dplyr/hflights.RData\")); library(dplyr); hflights <- tbl_df(hflights); "
  lst$DC_CODE <- "carriers <- hflights$TaxiIn"
  lst$DC_SOLUTION <- "carriers <- hflights$UniqueCarrier"
  
  lst$DC_SCT <- "test_object('carriers')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_object works with eq_condition", {
  lst <- list()
  lst$DC_CODE <- "\n    df.equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))\n    df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))"
  lst$DC_SOLUTION <- "\n    df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))\n    df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))"
  
  lst$DC_SCT <- "test_object(\"df.equiv\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"df.not_equiv\", eq_condition = \"equal\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"df.not_equiv\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"df.equiv\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object works with eq_condition 2", {
  lst <- list()
  lst$DC_CODE <- "\n    var.iden <- 3\n    var.equal <- 3 + 4.4e-8"
  lst$DC_SOLUTION = "\n    var.iden <- 3\n    var.equal <- 3"
  
  lst$DC_SCT <- "test_object(\"var.equal\", eq_condition = \"identical\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var.iden\", eq_condition = \"identical\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_object(\"var.equal\", eq_condition = \"equal\")"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object works with eval", {
  lst <- list()
  lst$DC_CODE <- "var <- 3\nvar.other <- 4"
  lst$DC_SOLUTION <- "var <- 5\nvar.other <- 3\nvar.not_here<-7\n"
  
  lst$DC_SCT <- "test_object(\"var\")"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var.not_here\", eval = FALSE)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_object(\"var\", eval = FALSE)"
  output <- test_it(lst)
  passes(output)
})

test_that("test_object passes along correct line numbers", {
  lst <- list()
  lst$DC_CODE <- "a <- 5\n\ny <- 4\n\nz <- 6\n\nz <- 7"
  lst$DC_SOLUTION <- "x <- 4\ny <- 5\nz <- 6"
  
  # If no assignments in student code, show nothing
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
  
  # If single assignment in student code, show line
  lst$DC_SCT <- "test_object('y')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 3, 3)
  
  # If two assignments in student code, show nothing
  lst$DC_SCT <- "test_object('z')"
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
})

test_that("test_object with line numbers - 2", {
  lst <- list()
  lst$DC_CODE <- "x <- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "x <- 5\ny <- 6\n7 ->> z"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "5 -> x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "x <<- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "5 ->> x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_CODE <- "5 ->> a\nx <- 6"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 2, 2)
  
  lst <- list()
  lst$DC_CODE <- "x = 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  
  lst <- list()
  lst$DC_PEC <- "x <- c(1, 2, 3)"
  lst$DC_CODE <- "x[x < 2] = 0"
  lst$DC_SOLUTION <- "x <- c(1, 2, 3)"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  line_info(output, 1, 1)
  
  # if x is used on the wrong side, shouldn't be taken as assignment
  lst <- list()
  lst$DC_CODE <- "x <- 5\ny <- x"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object('x')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
})

test_that("test_object inside MarkdownExercise doesn't show line numbers", {
  lst <- list()
  lst$DC_TYPE <- "MarkdownExercise"
  lst$DC_SOLUTION <- rjson::toJSON(list(my_solution.Rmd = "# This is a test\n```{r}\nx <- 5\n```\n"))
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "# This is a test\n```{r}\nx <- 5\n```\n"))
  lst$DC_SCT <- "test_rmd_group(2, test_object('x'))\nsuccess_msg(\"OK\")"
  lst$DC_FORMAT <- "PDF"
  lst$DC_ACTIVE_TAB <- "my_doc.Rmd"
  
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- rjson::toJSON(list(my_doc.Rmd = "# This is a test\n```{r}\nx <- 4\n```\n"))
  output <- test_it(lst)
  fails(output)
  line_info(output, NULL, NULL)
})

test_that("test_object works with an object which is the same but has different class", {
  lst <- list()
  lst$DC_SOLUTION <- "number <- which(c('a', 'b', 'c') == 'b')"
  lst$DC_CODE <-"number <- 2"
  lst$DC_SCT <- "test_object('number')"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <-"number <- 1"
  lst$DC_SCT <- "test_object('number')"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <-"number <- which(c('a', 'b', 'c') == 'c')"
  lst$DC_SCT <- "test_object('number')"
  output <- test_it(lst)
  fails(output)
})