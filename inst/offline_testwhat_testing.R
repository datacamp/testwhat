#offline testing of the testwhat package

###################################
library("testwhat")
library("datacampAPI")

clean_everything()

# Override library function
library <- function(package, ..., pos = NULL) {
  if (is.null(pos)) {
    pos <- grep("env:", search())
    pos <- if (length(pos) == 0) 2 else max(pos) + 1
  }
  base::library(package = as.character(substitute(package)), ..., character.only = TRUE, pos=pos)
}

get_output <- function(code) {
  output <- capture.output(source(file = textConnection(get_student_code()), print.eval = TRUE))
  if (inherits(output, "try-error")) {
    return("code contains an error")
  }
  return(paste(output, collapse=''))
}
####################################

assign('pre_exercise_code', '

',env=globalenv())


#SOLUTION CODE
set_solution_code('

pow_two <- function(x) {
    x*x
}

# Create a function sum_abs()
sum_abs <- function(x, y) {
  abs(x) + abs(y)
}


')

# USER CODE
set_student_code('

pow_two <- function(x) {
    x*x
}

# Create a function sum_abs()
sum_abs <- function(x) {
  abs(x) + abs(y)
}


')

sct = '

test_function_definition("sum_abs",
                         function_tests = {
                           have_a_look <- "Have another look at your implementation of the <code>sum_abs</code> function."
                           expect_that(sum_abs(1,2), equals(3), failure_msg = paste(have_a_look, "<code>sum_abs(1,2)</code> should equal 3."))
                           expect_that(sum_abs(-1,2), equals(3), failure_msg = paste(have_a_look, "<code>sum_abs(-1,2)</code> should equal 3."))
                           expect_that(sum_abs(1,-2), equals(3), failure_msg = paste(have_a_look, "<code>sum_abs(1,-2)</code> should equal 3."))
                           expect_that(sum_abs(-1,-2), equals(3), failure_msg = paste(have_a_look, "<code>sum_abs(-1,-2)</code> should equal 3."))
                         },
                         body_test = {
                           test_function("abs", not_called_msg = "You should use the <code>abs()</code> function twice in your definition of <code>sum_abs()</code>.")
                         })

'

###################################
eval(parse(text = pre_exercise_code), envir = globalenv())
res = try(eval(parse(text = get_student_code()), envir = globalenv()))
if(inherits(res, "try-error")) {
  set_student_error("there was an error")
} else {
  set_student_error(NULL)
}
set_student_output(get_output(get_student_code()))
eval(parse(text = pre_exercise_code),envir = get_solution_env())
eval(parse(text = get_solution_code()), envir = get_solution_env())

result = test_exercise(sct)
print(result)
###################################


