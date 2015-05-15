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
	train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"))
test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"))
library(rpart)
',env=globalenv())


#SOLUTION CODE
set_solution_code('

# Comparison of logicals
TRUE == FALSE

# Comparison of numerics
-6 * 14 != 17 - 101

# Comparison of character strings
"useR" == "user"

# Compare a logical with a numeric
TRUE == 1
')

# USER CODE
set_student_code('

# Comparison of logicals
TRUE == FALSE

# Comparison of numerics
17 - 101 != -6 * 14

# Comparison of character strings
"useR" == "user"

# Compare a logical with a numeric
TRUE == 1

')

sct = '

test_student_typed(strings = c("TRUE == FALSE", "FALSE == TRUE"), not_typed_msg = "Have another look at the comparison of logicals")
test_student_typed(strings = c("-6 * 14 != 17 - 101", "17 - 101 != -6 * 14"), not_typed_msg = "Have a closer look at the comparison of numerics (second instruction).")
test_student_typed(strings = c("\\"useR\\" == \\"user\\"", "\\"user\\" == \\"useR\\""), not_typed_msg = "Check your code for the comparison of character strings again.")
test_student_typed(strings = c("TRUE == 1", "1 == TRUE"), not_typed_msg = "Your code for comparing a logical with a numeric does not appear to be correct. Have another look.")

test_output_contains("FALSE", times = 3, incorrect_msg = "Your code should output <code>FALSE</code> three times. Have another look and make sure to print the results.")
test_output_contains("TRUE", times = 1, incorrect_msg = "Make sure to print the result of the last comparison.")

success_msg("Awesome! Since `TRUE` coerces to `1` under the hood, the last R expression you entered will evaluate to `TRUE`. Make sure not to mix up <code>==</code> (comparison) and <code>=</code> (assignment), <code>==</code> is the one you need to check the equality of R objects.")

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


