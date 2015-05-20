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

my_fun <- function(a, b) {
  a + b
}

')

# USER CODE
set_student_code('

my_fun <- function(c = 2, b = 3) {
  a + b + 1
}
  
')

sct = '

test_function_definition(name = "my_fun", function_tests = { expect_that(my_fun(1,2), equals(3), failure_msg = "1 + 2 should be 3") },arguments = c("a","b"),check_defaults = FALSE,body_sct = test_student_typed("a + b")) 
success_msg("OKE!")
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


