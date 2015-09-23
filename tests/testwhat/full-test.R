library(RAutomatedTesting)

if (file.exists("results.log")) file.remove("results.log")

for (filename in dir("scenarios/")) {
  if (substring(filename, 1, 5) == "test-" && 
      substring(filename, nchar(filename)-1) == ".R") {
    source(paste0("scenarios/",filename))
    write(filename, file = "results.log", append = TRUE)
    output <- test_all_scenarios(scen)
    write(paste(output,collapse="\n"), file = "results.log", append = TRUE)
  }
}