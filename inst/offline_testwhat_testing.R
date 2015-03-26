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

chol <- read.table(url("http://assets.datacamp.com/blog_assets/chol.txt"), header = TRUE)
chol$BMI <- chol$WEIGHT/(chol$HEIGHT/100)^2
hist(chol$BMI)

')

# USER CODE
set_student_code('

# chol <- read.table(url("http://assets.datacamp.com/blog_assets/chol.txt"), header = TRUE)
# chol$BMI <- chol$WEIGHT/(chol$HEIGHT/100)^2 - 4
# hist(chol$BMI)

')

sct = '

test_instruction(1, {
  test_function("url", "description")
  test_that("checks on chol being loaded correctly", {
    expect_that(exists("chol", envir = globalenv(), inherits = FALSE), is_true())
    # expect_that("AGE" %in% names(chol), is_true())
  })
})
test_instruction(2, {
  test_object("chol")
})
test_instruction(3, {
  test_error()
  test_function("hist", "x")
})
success_msg("Wonderful! Sure you dont want to become a doctor?!")

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

result = test_exercise(sct, "challenge")
print(result)
###################################


