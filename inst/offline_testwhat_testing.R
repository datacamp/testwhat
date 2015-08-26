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

a <- list("some code", 2, data.frame(a=c(1,2,3), b=c(4,5,6)))
test <- 3
')

# USER CODE
set_student_code('
b <- data.frame(c=c(1,2,3), d=c(4,5,6))
test <- list("some code", 2, b)
')

sct = '

test_an_object("a", "You didn\'t define the thing", eq_condition = "equivalent")
test_object("test", eval = TRUE)


success_msg("Perfect! Know that there are many more functions to create the plots that you need in R!")

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


