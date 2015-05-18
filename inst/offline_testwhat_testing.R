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

# Variables related to your last day of recordings
medium <- "LinkedIn"
num_views <- 14

# Examine the if statement for medium
if(medium == "LinkedIn") {
  print("Showing LinkedIn information")
}

# Write the if statement for num_views
if(num_views > 15) {
  print("You\'re popular!")
}

')

# USER CODE
set_student_code('

# Variables related to your last day of recordings
medium <- "LinkedIn"
num_views <- 14

# Examine the if statement for medium
if(medium == "LinkedIn") {
  print("Showing LinkedIn information")
}

# Write the if statement for num_views
if(num_views > 15) {
  
}
')

sct = '

test_error()

test_if_else(index = 1, 
if_cond_test = {
test_student_typed(c("medium == \\"LinkedIn\\"", "\\"LinkedIn\\" == medium"),
not_typed_msg = "Have another look at the <code>condition</code> part of the first <code>if</code> statement. It was already coded for you!")
},
if_expr_test = {
msg <- "Don\'t change anything about the printout of the message in the first <code>if</code> statement. It has already been coded for you!"
test_function("print", "x", not_called_msg = msg, incorrect_msg = msg)
})

test_if_else(index = 2,
if_cond_test = {
test_student_typed(c("num_views > 15", "15 < num_views"),
not_typed_msg = "Have another look at the <code>condition</code> part of your second <code>if</code> statement. You should use <code>num_views</code>!")
},
if_expr_test = {
msg <- "Use the function <code>print()</code> to print the message <code>\\"You\'re popular!\\"</code> when the number of views exceeds 15."
test_function("print", "x", not_called_msg = msg, incorrect_msg = msg)
})

success_msg("Great! Try to see what happens if you change the <code>medium</code> and <code>num_views</code> variables and run your code again. Let\'s further customize these if statements in the next exercise.")
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


