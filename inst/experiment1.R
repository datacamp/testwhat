
init_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "init", 
                                            DC_PEC = "library(ggvis)", 
                                            DC_SOLUTION = "mtcars %>% ggvis(~wt, ~hp) %>% layer_points()\nmtcars %>% ggvis(~mpg, ~cyl) %>% layer_points",
                                            DC_SCT = "test_props(1, funs = 'ggvis')", 
                                            DC_TYPE = "NormalExercise", 
                                            DC_ECHO = TRUE))))
submit_output <- rjson::fromJSON(RBackend::execute(rjson::toJSON(list(DC_COMMAND = "submit", 
                                              DC_TYPE = "NormalExercise",
                                              DC_CODE = "mtcars %>% ggvis(~wt, ~hp) %>% layer_points()\nmtcars %>% ggvis(~mpg, ~cyl) %>% layer_points"))))
str(tail(submit_output, 1))
