library(RBackend)
library(rjson)
init_output <- fromJSON(execute(toJSON(list(DC_COMMAND = "init", 
                                            DC_PEC = "", 
                                            DC_SOLUTION = '

x <- 4

', 
                                            DC_SCT = '

test_object("x")

', 
                                            DC_TYPE = "NormalExercise", 
                                            DC_ECHO = TRUE))))
submit_output <- fromJSON(execute(toJSON(list(DC_COMMAND = "submit", 
                                              DC_TYPE = "NormalExercise",
                                              DC_CODE ='


x <- 3

                                              '))))
str(tail(submit_output, 1))

