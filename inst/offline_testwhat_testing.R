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
	train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"))
  
  # Assing the testing set
  test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"))
',env=globalenv())


#SOLUTION CODE
set_solution_code('

# Create the variable child, and indicate whether child or no child
train$Child <- 0
train$Child[train$Age < 18] <- 1

# two way comparison
table(train$Child, train$Survived)
prop.table(table(train$Child, train$Survived),1)

')

# USER CODE
set_student_code('

# Create the variable child, and indicate whether child or no child
train$Child <- 0
train$Child[train$Age < 18] <- 1

# two way comparison
table(train$Child, train$Survived)
prop.table(table(train$Child, train$Survived),1)

')

sct = '

test_data_frame("train","Child")
test_object("train")
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


