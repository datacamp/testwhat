
init_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "init", 
                                            DC_PEC = "", 
                                            DC_SOLUTION = "mean(x = 1:10)",
                                            DC_SCT = "test_function('mean', args = 'x')", 
                                            DC_TYPE = "NormalExercise", 
                                            DC_ECHO = TRUE))))
submit_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "submit", 
                                              DC_TYPE = "NormalExercise",
                                            DC_CODE = "mean(1: 9,\nna.rm = T,\ntrim = 0.1)"))))
str(tail(submit_output, 1))