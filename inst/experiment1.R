library(RBackend)
lst <- list(DC_TYPE = "NormalExercise",
            DC_ECHO = TRUE,
            DC_PEC = "",
            DC_SOLUTION = "x <- mean(1:3)\ny <- mean(c(1:3, NA), na.rm = TRUE)",
            DC_COMMAND = "init",
            DC_SCT = "test_function('mean', args = 'x', index = 1)\ntest_function('mean', args = 'x', index = 2)\ntest_function('mean', args = 'x', index = 3)")
output <- rjson::fromJSON(execute(rjson::toJSON(lst)))
lst <- list(DC_TYPE = "NormalExercise",
            DC_ECHO = TRUE,
            DC_CODE = "x <- mean(1:3)\ny <- mean(c(1:3, NA), na.rm = TRUE)",
            DC_COMMAND = "submit")
output <- rjson::fromJSON(execute(rjson::toJSON(lst)))
