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

pow_two <- function(x, print_info = TRUE) {
  y <- x^2
  if(print_info) {
    print(paste(x, "to the power two equals", y))
  }
  y
}

')

# USER CODE
set_student_code('

pow_two <- function(x, print_info = TRUE) {
  y <- x^2
  if(print_info) {
    print(paste(x, "to the power two equals", y))
  }
  y
}

')

sct = '


test_function_definition(name = "pow_two", 
                         function_test = {
                           have_a_look <- "Check your code to extend the <code>pow_two()</code> function again."
                           expect_that(pow_two(3, FALSE), equals(9), failure_msg = paste(have_a_look, "<code>pow_two(3,FALSE)</code> should return 9."))
                           expect_that(pow_two(3, TRUE), equals(9), failure_msg = paste(have_a_look, "<code>pow_two(3,TRUE)</code> should return 9."))
                           expect_that(grepl("3 to the power two equals 9", paste(capture.output(pow_two(3,TRUE)), collapse = "\n")), is_true(),
                                       failure_msg = paste(have_a_look, "<code>pow_two(3,TRUE)</code> should return 9 and print out \\"3 to the power two equals 9\\"."))
                         })


success_msg("Truly impressive functionality! Head over to the next exercise.")

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


