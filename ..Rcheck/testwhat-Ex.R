pkgname <- "testwhat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "testwhat-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('testwhat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("test_an_object")
### * test_an_object

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_an_object
### Title: Check if the student defined an object, independent of the name
### Aliases: test_an_object

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # x <- 5
##D 
##D # sct command to test whether student defined _an_ object with same value
##D test_an_object("x")
##D 
##D # All of the following student submissions are accepted
##D # x <- 5
##D # y <- 5
##D # z <- 4 + 1 + 1e-8
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_an_object", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_correct")
### * test_correct

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_correct
### Title: Test things. If it fails, test additional things.
### Aliases: test_correct

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # x <- mean(1:3, na.rm = TRUE)
##D 
##D # Example SCT
##D test_correct({
##D  test_object("x")
##D }, {
##D  # this code only is run if test_object("x") fails
##D  test_function("mean", "x")
##D  # test_object("x") is automatically run again to generate a fail if test_function passed.
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_correct", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_data_frame")
### * test_data_frame

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_data_frame
### Title: Test list elements (or data frame columns)
### Aliases: test_data_frame

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # df <- data.frame(a = 1:3, b = LETTERS[1:3])
##D 
##D # sct command to test column a
##D test_data_frame("df", columns = "a")
##D 
##D # sct command to test column b
##D test_data_frame("df", columns = "b") 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_data_frame", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_error")
### * test_error

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_error
### Title: Check whether the student's submission threw an error.
### Aliases: test_error

### ** Examples

## Not run: 
##D # Example student code: x <- 4 + "a"
##D 
##D # R error message as feedback:
##D test_error()
##D 
##D # R error message as feedback, with additional info:
##D test_error("Don't sum numerics and characters!")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_error", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_expression_output")
### * test_expression_output

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_expression_output
### Title: Test output of expression
### Aliases: test_expression_output

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # my_fun <- function(a, b) { a + b }
##D 
##D # Test whether my_fun(1,2) and my_fun(1,2)
##D # give same _output_
##D test_function_definition({
##D  test_expression_output(my_fun(1,2))
##D  test_expression_output(my_fun(-1,-2))
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_expression_output", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_expression_result")
### * test_expression_result

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_expression_result
### Title: Test result of expression
### Aliases: test_expression_result

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # my_fun <- function(a, b) { a + b }
##D 
##D # Test whether my_fun(1,2) and my_fun(1,2)
##D # give same _result_
##D test_function_definition({
##D  test_expression_result(my_fun(1,2))
##D  test_expression_result(my_fun(-1,-2))
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_expression_result", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_file_exists")
### * test_file_exists

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_file_exists
### Title: Test whether a file exists
### Aliases: test_file_exists

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # write("hello", file = "test.txt")
##D 
##D # SCT to test if file exists
##D test_file_exists("test.txt")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_file_exists", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_for_loop")
### * test_for_loop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_for_loop
### Title: Test a for loop
### Aliases: test_for_loop

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D for(i in 1:5) {
##D  print("hurray!") 
##D }
##D 
##D # SCT to test this loop:
##D test_for_loop({
##D  test_student_typed("in")
##D  test_student_typed("1")
##D  test_student_typed("5")
##D }, {
##D  test_function("print")
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_for_loop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_function")
### * test_function

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_function
### Title: Test whether a student correctly called a function
### Aliases: test_function test_function_v2

### ** Examples

## Not run: 
##D # Suppose the solution contains: mean(1:3, na.rm = TRUE)
##D # To test this submission, provide the following in the sct
##D test_function("mean", c("x", "na.rm"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_function", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_function_definition")
### * test_function_definition

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_function_definition
### Title: Check whether the student defined a function correctly
### Aliases: test_function_definition

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # my_fun <- function(a, b) { a + b }
##D 
##D # SCT testing both result and printouts:
##D test_function_definition({
##D  test_expression_result(my_fun(1,2))
##D  test_expression_output(my_fun(1,2))
##D }, {
##D  test_student_typed("+")
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_function_definition", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_if_else")
### * test_if_else

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_if_else
### Title: Test a conditional statement
### Aliases: test_if_else

### ** Examples

## Not run: 
##D # Example solution code
##D vec <- c("a", "b", "c")
##D if("a" %in% vec) {
##D  print("a in here")
##D } else if(any("b" > vec)) {
##D  cat("b not smallest")
##D } else {
##D  str(vec)
##D }
##D 
##D # SCT to test this loop
##D test_if_else({
##D  test_student_typed("%in%")
##D }, {
##D  # test if expr part
##D  test_function("print")
##D }, {
##D  # test else expr part
##D  test_if_else({
##D    # test cond part of else if
##D    test_student_typed(">")
##D  }, {
##D    # test else if expr part
##D    test_function("cat")
##D  }, {
##D    # test else part
##D    test_function("str")
##D  })
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_if_else", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_library_function")
### * test_library_function

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_library_function
### Title: Test whether the library function was called correctly
### Aliases: test_library_function

### ** Examples

## Not run: 
##D # Example solution code
##D library(ggvis)
##D library(dplyr)
##D 
##D # SCT to test both library calls:
##D test_library_function("ggvis")
##D test_library_function("dplyr")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_library_function", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_mc")
### * test_mc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_mc
### Title: Test a multiple choice exercise
### Aliases: test_mc

### ** Examples

## Not run: 
##D # Example solution: second instruction correct.
##D 
##D # Corresponding SCT:
##D msg1 <- "Not good, try again!"
##D msg2 <- "Nice one!"
##D msg3 <- "Not quite, give it another shot."
##D msg4 <- "Don't be silly..."
##D test_mc(2, feedback_msgs = c(msg1, msg2, msg3, msg4))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_mc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_object")
### * test_object

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_object
### Title: Test R object existence and value
### Aliases: test_object

### ** Examples

## Not run: 
##D # Example 1 solution code:
##D # x <- mean(1:3, na.rm = TRUE)
##D 
##D # sct command to test existence and value of x:
##D test_object("x")
##D 
##D # sct command to test only existence of x:
##D test_object("x", eval = FALSE)
##D 
##D # Example 2 solution code:
##D # y <- list(a = 2, b = 3, c = 4)
##D 
##D # Small numerical difference allowed + no check on attributes
##D test_object(y)
##D 
##D # Small numerical difference allowed + check attributes
##D test_object(y, eq_condition = "equals")
##D 
##D # No numerical difference allowed + check attributes
##D test_object(y, eq_condtion = "identical")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_object", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_or")
### * test_or

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_or
### Title: Test if one of the given sct parts are correct.
### Aliases: test_or

### ** Examples

## Not run: 
##D   # test if either the object a or the object b is correct
##D   test_or(test_object("a"), test_object("b"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_or", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_output_contains")
### * test_output_contains

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_output_contains
### Title: Check whether the student printed something to the console
### Aliases: test_output_contains

### ** Examples

## Not run: 
##D # SCT to test whether student printed numbers 1 to 10
##D test_output_contains("for(i in 1:10) print(i)")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_output_contains", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_student_typed")
### * test_student_typed

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_student_typed
### Title: Test student's submission as text
### Aliases: test_student_typed

### ** Examples

## Not run: 
##D # Example solution code: TRUE & FALSE
##D 
##D # SCT to test this as a string (both T & F and F & T should be accepted)
##D test_student_typed(c("TRUE & FALSE", "FALSE & TRUE"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_student_typed", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_while_loop")
### * test_while_loop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_while_loop
### Title: Test a while loop
### Aliases: test_while_loop

### ** Examples

## Not run: 
##D # Example solution code:
##D while(x < 18) {
##D  x <- x + 5
##D  print(x)
##D }
##D 
##D # SCT to test this loop:
##D test_while_loop({
##D  test_student_typed(c("< 18", "18 >"))
##D }, {
##D  test_student_Typed(c("x + 5", "5 = x"))
##D  test_function("print", eval = FALSE) # no actual value matching possible!!
##D })
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_while_loop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
