# specify that a certain function or operator has been used
set_used <- function(name, stud_index, sol_index) {
  tw$set(fun_usage = c(tw$get("fun_usage"), 
                       list(list(name = name, 
                                 stud_index = stud_index, 
                                 sol_index = sol_index))))
}

# get the indexes that haven't covered any previous tests
get_seq <- function(name, stud_indices, sol_index) {
  fu <- tw$get("fun_usage")
  name_hits <- sapply(fu, `[[`, "name") == name
  fu <- fu[name_hits]
  sol_index_hits <- sapply(fu, `[[`, "sol_index") == sol_index
  if (any(sol_index_hits)) {
    fu <- fu[sol_index_hits]
    fu[[1]]$stud_index
  } else {
    setdiff(stud_indices, sapply(fu, `[[`, "stud_index"))
  }
}