library(RAutomatedTesting)

scen <- list(
  list(
    type = "NormalExercise",
    pre_exercise_code = "library(ggplot2)",
    student = "
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_smooth(se = FALSE) + geom_point(aes(alpha = disp), size = 5) + geom_point( size = 5, alpha = 0.3)
    ",
    solution = "
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point(alpha = 0.3, size = 5) + geom_point(size = 5, aes(alpha = disp)) + geom_smooth(se = FALSE)
    ",
    pass = list(
      test_correct_data = list(
        long = "test succeeds if command has the correct data",
        sct = "
test_ggplot()
        "
      )
    )
  )
)

test_all_scenarios(scen)