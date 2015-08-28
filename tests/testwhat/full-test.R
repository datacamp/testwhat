rm(list = ls(globalenv()), envir = globalenv())
rm(list = ls(get_solution_env()), envir = get_solution_env())

library(testwhat)
library(datacampAPI)

source("reporter-full-test.R")
set_reporter(FullTestReporter$new())

cat(bold("Starting testwhat testing\n=========================\n\n\n"))

for (filename in dir()) {
  if (substring(filename, 1, 5) == "test-" && 
      substring(filename, nchar(filename)-1) == ".R") {
        cat(blue(bold(paste0("Testing ", substring(filename, 6, nchar(filename)-2), ":\n\n"))))
        source(filename)
        cat("\n\n")
  }
}