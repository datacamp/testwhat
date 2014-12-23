#offline testing of the testwhat package

###################################
library("testwhat")

rm(list = ls())
rm(list = ls(envir = get_solution_env(), all.names = TRUE),
   envir = get_solution_env(), inherits = FALSE)

#' Override library function
library <- function(package, ..., pos = NULL) {
  if (is.null(pos)) {
    pos <- grep("env:", search())
    pos <- if (length(pos) == 0) 2 else max(pos) + 1
  }
  base::library(package = as.character(substitute(package)), ..., character.only = TRUE, pos=pos)
}

get_output <- function(code) {
  output = try(capture.output(try(eval(parse(text=code)))))
  if (inherits(output, "try-error")) {
    return("code contains an error")
  }
  return(paste(output, collapse=''))
}
####################################

assign('pre_exercise_code', '
	# pre exercise code comes here
',env=globalenv())


#SOLUTION CODE
set_solution_code('

x <- mean(1:5)
print(x)

')

# USER CODE
set_student_code('

x <- mean(1:3)
x

')

sct = '

test_error()
test_output_contains("x",incorrect_msg = "did not print x")
test_object("x")
test_function("mean","x")

success_msg("Great job! Continue to the next exercise!")
'


###################################
eval(parse(text = pre_exercise_code), envir = globalenv())
res = try(eval(parse(text = get_student_code()), envir = globalenv()))
if(inherits(res, "try-error")) {
  set_student_error(TRUE)
} else {
  set_student_error(FALSE)
}
set_student_output(get_output(get_student_code()))
eval(parse(text = pre_exercise_code),envir = get_solution_env())
eval(parse(text = get_solution_code()), envir = get_solution_env())

result = test_exercise(sct)
print(result)
###################################


