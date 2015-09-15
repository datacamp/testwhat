library(testwhat)
library(datacampAPI)

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

test_scenario <- function(name, 
                          pre_ex= '', student = '', solution = '',
                          msg = NULL, 
                          passes = NULL) {
  if (is.list(name)) {
    if (!is.list(passes)) {
      stop("if 'name' is a list, 'passes' should be a list as well.")
    }
    if (!is.null(msg) && !is.list(msg)) {
      stop("if 'name' is a list, 'msg' should be NULL or a list.")
    }
  } else if (!is.list(msg) && !is.list(passes)) {
    name <- list(name)
    msg <- list(msg)
    passes <- list(passes)
  } else {
    stop("if 'passes' or 'msg' is a list, 'name' should be a list as well.")
  }

  saved_global_env <- ls(globalenv())
  saved_solution_env <- ls(get_solution_env())
  set_solution_code(solution)
  set_student_code(student)
  log <- capture.output(eval(parse(text = pre_ex), envir = globalenv()))
  log <- capture.output(try(eval(parse(text = get_student_code()), envir = globalenv())))
  if(inherits(log, "try-error")) {
    set_student_error("there was an error")
  } else {
    set_student_error(NULL)
  }
  set_student_output(get_output(get_student_code()))
  log <- capture.output(eval(parse(text = pre_ex),envir = get_solution_env()))
  log <- capture.output(eval(parse(text = get_solution_code()), envir = get_solution_env()))
  
  rm(log)
  
  for (i in 1:length(name)) {
    descr <- ifelse(is.null(msg[i]), name[i], paste0(name[i],' - ',msg[i]))
    get_reporter()$start_high_level_test(descr)
    if (is.function(passes[[i]])) {
      passes[[i]]()
    } else {
      eval(passes[[i]])
    }
    get_reporter()$end_high_level_test()
  }
  
  rm(list=ls(globalenv())[!(ls(globalenv()) %in% saved_global_env)], envir=globalenv())
  rm(list=ls(get_solution_env())[!(ls(get_solution_env()) %in% saved_solution_env)], envir=get_solution_env())
}

test_rmd_scenario <- function(name,
                              student = '', solution = '',
                              msg = NULL, 
                              passes = NULL) {
  if (is.list(name)) {
    if (!is.list(passes)) {
      stop("if 'name' is a list, 'passes' should be a list as well.")
    }
    if (!is.null(msg) && !is.list(msg)) {
      stop("if 'name' is a list, 'msg' should be NULL or a list.")
    }
  } else if (!is.list(msg) && !is.list(passes)) {
    name <- list(name)
    msg <- list(msg)
    passes <- list(passes)
  } else {
    stop("if 'passes' or 'msg' is a list, 'name' should be a list as well.")
  }
  
  saved_global_env <- ls(globalenv())
  saved_solution_env <- ls(get_solution_env())
  clean_sct_env()

  set_solution_code(solution)
  set_student_code(student)
  
  for (i in 1:length(name)) {
    descr <- ifelse(is.null(msg[i]), name[i], paste0(name[i],' - ',msg[i]))
    get_reporter()$start_high_level_test(descr)
    if (is.function(passes[[i]])) {
      log <- capture.output(passes[[i]]())
    } else {
      log <- capture.output(eval(passes[[i]]))
    }
    get_reporter()$end_high_level_test()
  }
  
  rm(list=ls(globalenv())[!(ls(globalenv()) %in% saved_global_env)], envir=globalenv())
  rm(list=ls(get_solution_env())[!(ls(get_solution_env()) %in% saved_solution_env)], envir=get_solution_env())
}

test_call <- function(name, 
                      msg = NULL,
                      passes = NULL) {
  
  descr <- ifelse(is.null(msg), name, paste0(name,' - ',msg))
  get_reporter()$start_high_level_test(descr)
  if (is.function(passes)) {
    passes()
  } else {
    eval(passes)
  }
  get_reporter()$end_high_level_test()
}

expect_fail <- function(code, msg = NULL) {
  get_reporter()$toggle_fail(TRUE, msg)
  code
  get_reporter()$toggle_fail(FALSE)
}
  