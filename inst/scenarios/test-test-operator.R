scen <- list(
  list(
    type = "NormalExercise",
    pre_exercise_code = '',
    student = '
# Load ggplot2:
library(ggplot2)

# Explore the mtcars data frame with str()
str(mtcars)

# Execute the following command
ggplot(mtcars, aes(x = cyl, y = mpg)) +
geom_point()

# Change the command below so that cyl is treated as a factor
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
geom_point()
    ',
    solution = '
# Load ggplot2:
library(ggplot2)

# Explore the mtcars data frame with str()
str(mtcars)

# Execute the following command
ggplot(mtcars, aes(x = cyl, y = mpg)) +
geom_point() + geom_point()

# Change the command below so that cyl is treated as a factor
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
geom_point()
    ',
    pass = list(
      test_ggplot = list(
        long = "Test some ggplot2 functionality.",
        sct = '
test_operator("+", 1, {
  test_function_v2("ggplot", c("data", "mapping"))
},{
  test_function("geom_point")
})


success_msg("Good job! Notice that ggplot2 treats cyl as a continuous variable. We get a plot, but it\'s not quite right, because it gives the impression that there is such a thing as a 5 or 7-cylinder car, which there is not.")
        '
      )
    )
  )
)