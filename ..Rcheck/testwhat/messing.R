library(RBackend)
library(rjson)
init_output <- fromJSON(execute(toJSON(list(DC_COMMAND = "init", 
                                            DC_PEC = "", 
                                            DC_SOLUTION = "



", 
                                            DC_SCT = "test_error()", 
                                            DC_TYPE = "NormalExercise", 
                                            DC_ECHO = TRUE))))
submit_output <- fromJSON(execute(toJSON(list(DC_COMMAND = "submit", 
                                              DC_TYPE = "NormalExercise",
                                              DC_CODE ="


b <- 4
a <- b
rm(b)
a <- b
print(a)

                                              "))))
str(tail(submit_output, 1))

