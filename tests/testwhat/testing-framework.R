library("testwhat")
library("datacampAPI")

get_output <- function(code) {
  output <- capture.output(source(file = textConnection(get_student_code()), print.eval = TRUE))
  if (inherits(output, "try-error")) {
    return("code contains an error")
  }
  return(paste(output, collapse=''))
}

library <- function(package, ..., pos = NULL) {
  if (is.null(pos)) {
    pos <- grep("env:", search())
    pos <- if (length(pos) == 0) 2 else max(pos) + 1
  }
  base::library(package = as.character(substitute(package)), ..., character.only = TRUE, pos=pos)
}

set_scenarios <- function(scenarios) {
  scenarios <- scenarios
}

test_scenario <- function(description, scenario.id, code) {
  saved_global_env <- ls(globalenv())
  saved_solution_env <- ls(get_solution_env())
  with(get(scenario.id, envir = scenarios), {
    set_solution_code(solution)
    set_student_code(student)
    eval(parse(text = pre_ex), envir = globalenv())
    res <- try(eval(parse(text = get_student_code()), envir = globalenv()))
    if(inherits(res, "try-error")) {
      set_student_error("there was an error")
    } else {
      set_student_error(NULL)
    }
    rm(res)
    set_student_output(get_output(get_student_code()))
    eval(parse(text = pre_ex),envir = get_solution_env())
    eval(parse(text = get_solution_code()), envir = get_solution_env())
  })
  eval(code)
  rm(list=ls(globalenv())[!(ls(globalenv()) %in% saved_global_env)], envir=globalenv())
  rm(list=ls(get_solution_env())[!(ls(get_solution_env()) %in% saved_solution_env)], envir=get_solution_env())
  print(paste0("PASSED: ", description))
}

test_call <- function(description, code) {
  tryCatch(code,
           error = function(e) {
             stop(description)
           })
  print(paste0("PASSED: ",description))
}