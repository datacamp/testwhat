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
movies <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/introduction_to_r/movies.csv", header = TRUE, stringsAsFactors = FALSE)
movies$genre <- factor(movies$genre)
# movies is already pre-loaded

# Create a boxplot of the runtime variable
boxplot(movies$runtime)

# Subset the dateframe and plot it entirely
plot(movies[,c("rating", "votes", "runtime")] )

# Create a pie chart of the table of counts of the genres
pie(table(movies$genre))

test = 3
')

# USER CODE
set_student_code('
movies <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/introduction_to_r/movies.csv", header = TRUE, stringsAsFactors = FALSE)
movies$genre <- factor(movies$genre)
# movies is already pre-loaded

# Create a boxplot of the runtime variable
boxplot(movies$runtime)

# Subset the dateframe and plot it entirely
plot(movies[,c("rating", "votes", "runtime")] )

# Create a pie chart of the table of counts of the genres
pie(table(movies$genre))

test <- 3
')

sct = '
test_error()
msg <- "Do not overwrite the <code>movies</code> data frame; it has already been created for you!"
test_object("movies", undefined_msg = msg, incorrect_msg = msg)

test_function("boxplot", "x",
              incorrect_msg = paste("Have you correctly possed the <code>runtime</code> variable of",
                                    "<code>movies</code> to the <code>boxplot()</code> function?"))

test_function("plot", "x", 
              incorrect_msg = paste("Have you passed the correct data frame to the <code>plot()</code> function?",
                                    "Make sure that it\'s an entire data frame, containing the three columns in the listed order!"))

test_function("pie", "x", 
              incorrect_msg = paste("Have you created a table of genre counts and subsequently",
                                    "passed it to the <code>pie()</code> function? Try again."))

test_object("test")


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


