library(RBackend)
library(rjson)
init_output <- fromJSON(execute(toJSON(list(DC_COMMAND = "init", 
                                            DC_PEC = "", 
                                            DC_SOLUTION = '

# https URL to the wine RData file.
url_rdata <- "https://s3.amazonaws.com/assets.datacamp.com/course/importing_data_into_r/wine.RData"
                                            
                                            # Download the wine file to your working directory
                                            download.file(url_rdata, destfile = "wine_local.RData")
                                            
                                            # Load the wine data into your workspace using load()
                                            load("wine_local.RData")
                                            
                                            # Print out the summary of the wine data
                                            summary(wine)

', 
                                            DC_SCT = '

test_error()

msg <- "Dont change or remove the link we gave you."
test_object("url_rdata", incorrect_msg = msg, undefined_msg = msg)


test_file_exists("wine_local.RData",incorrect_msg = paste("Could "))
test_function("download.file", "url")
test_function("download.file", "destfile")

msg <- paste("asdf")
test_object("wine", incorrect_msg = msg, undefined_msg = msg)
test_function("load", "file", incorrect_msg = msg)


test_correct({
  test_output_contains("summary(wine)")
                                            },{
                                            test_function("summary", "object",
                                            not_called_msg = "Use [`summary()`](http://www.rdocumentation.org/packages/base/functions/summary) to print out a summary of the wine data frame.",
                                            incorrect_msg = "Have you passed the correct variable name, `wine`, to [`summary()`](http://www.rdocumentation.org/packages/base/functions/summary)?")
                                            })
test_error()

success_msg("Great! Another way to load remote `RData` files is to use the [`url()`](http://www.rdocumentation.org/packages/base/functions/connections) function inside [`load()`](http://www.rdocumentation.org/packages/base/functions/load). However, this will not save the `RData` file to a local file.")

', 
                                            DC_TYPE = "NormalExercise", 
                                            DC_ECHO = TRUE))))
submit_output <- fromJSON(execute(toJSON(list(DC_COMMAND = "submit", 
                                              DC_TYPE = "NormalExercise",
                                              DC_CODE ='


# https URL to the wine RData file.
url_rdata <- "https://s3.amazonaws.com/assets.datacamp.com/course/importing_data_into_r/wine.RData"
                                              
# Download the wine file to your working directory
download.file(url_rdata, destfile = "wine_local.RData")

# Load the wine data into your workspace using load()
load("wine_local.RData")


                                              '))))
str(tail(submit_output, 1))

