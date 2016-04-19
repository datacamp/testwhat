
init_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "init", 
                                            DC_PEC = "", 
                                            DC_SOLUTION = '

x <- 4

', 
                                            DC_SCT = '

test_object("x")

', 
                                            DC_TYPE = "NormalExercise", 
                                            DC_ECHO = TRUE))))
submit_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "submit", 
                                              DC_TYPE = "NormalExercise",
                                              DC_CODE ='


y <- 3

                                              '))))
str(tail(submit_output, 1))

