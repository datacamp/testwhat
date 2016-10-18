library(RBackend)
lst <- list(DC_TYPE = "NormalExercise",
            DC_ECHO = TRUE,
            DC_PEC = "",
            DC_SOLUTION = "x <- 10",
            DC_COMMAND = "init",
            DC_SCT = "test_object('x')")
output <- rjson::fromJSON(execute(rjson::toJSON(lst)))
lst <- list(DC_TYPE = "NormalExercise",
            DC_CODE = "x <- 8",
            DC_COMMAND = "submit")
output <- rjson::fromJSON(execute(rjson::toJSON(lst)))
