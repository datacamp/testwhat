library(RAutomatedTesting)

for (filename in dir("new_format/")) {
  if (substring(filename, 1, 5) == "test-" && 
      substring(filename, nchar(filename)-1) == ".R") {
    source(paste0("new_format/",filename))
    write(filename, file = "results.log", append = TRUE)
    results <- test_all_scenarios(scen)
    if (length(results) != 0) {
      write(unlist(results), file = "results.log", append = TRUE)
    } else {
      write("ALL PASS", file = "results.log", append = TRUE)
    }
  }
}

