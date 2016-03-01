library(RBackend)
input <- list(DC_TYPE = "NormalExercise", DC_PEC = "", DC_SOLUTION = "x <- 5", DC_CODE = "x <- 6", DC_SCT = "test_object('x')")
input$DC_COMMAND <- "init"
init_result <- fromJSON(execute(toJSON(input)))
input$DC_COMMAND <- "submit"
submit_result <- fromJSON(execute(toJSON(input)))
str(submit_result)