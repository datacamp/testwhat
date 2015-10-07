test_everything <- function() {
  oldwd <- getwd()
  log_path <- file.path(oldwd, "results.log")
  if (file.exists(log_path)) file.remove(log_path)
  setwd(file.path(system.file(package = "testwhat")))
  for (filename in dir("scenarios/")) {
    if (substring(filename, 1, 5) == "test-" && 
        substring(filename, nchar(filename)-1) == ".R") {
      source(file.path("scenarios/",filename))
      write(paste0(">> ", filename), file = log_path, append = TRUE)
      output <- test_all_scenarios(scen)
      write(paste(output,collapse="\n"), file = log_path, append = TRUE)
    }
  }
  
  # message("The results of the tests can be found here:\n", log_path)
  setwd(oldwd)  
}

library(RAutomatedTesting)
test_everything()