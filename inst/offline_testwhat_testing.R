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
options(lang = "fr")
',env=globalenv())


#SOLUTION CODE
set_solution_code('

y <- 8
x <- 5
c(x,y)
plot(x)

')

# USER CODE
set_student_code('

y <- 8
x <- 5
c(x,y)
plot(6)


')

sct = '


test_object("y")
test_object("x")
test_output_contains("c(x,y)")

test_function("plot", c("x"), index = 4)
success_msg("OK")

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


