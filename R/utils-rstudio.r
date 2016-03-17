get_clean_lines <- function(code) {
  # convert summarize to summarise. (hacky solution)
  code = gsub("summarize","summarise",code)
  
  pd <- getParseData(parse(text = code, keep.source = TRUE))
  exprids <- pd$id[pd$parent == 0 & pd$token != "COMMENT" & pd$token != "';'"]
  codelines <- sapply(exprids, function(x) getParseText(pd, id = x))
  
  cleanlines <- sapply(codelines, clean_unpipe, USE.NAMES = FALSE)
  
  # remove "obj = " assignments: delete "=" lines and the ones preceding
  eqsubsids = which(cleanlines == "=", TRUE)
  if(length(eqsubsids) == 0) {
    return(cleanlines)
  } else {
    removeids = c(eqsubsids, eqsubsids-1)
    return(cleanlines[-removeids])
  }
}

# subfunction used by get_clean_lines to remove the pipe operator.
clean_unpipe <- function(code) {
  if(grepl("%>%",code)) {
    return(paste0(deparse(unpipe(as.call(parse(text=code))[[1]])),collapse = ''))
  } else {
    return(code)
  }
}


create_student_pd <- function(student_code = NULL) {
  if(is.null(student_code)) {
    stop("The student_code argument cannot be empty")
  }
  student_code_parts = get_clean_lines(code = student_code)
  stud = lapply(student_code_parts, function(x) getParseData(parse(text = x, keep.source = TRUE)))
  return(stud)
}

create_solution_pd <- function(solution_code = NULL) {
  if(is.null(solution_code)) {
    stop("The solution_code argument cannot be empty")
  }
  solution_code_parts = get_clean_lines(code = solution_code)
  sol = lapply(solution_code_parts, function(x) getParseData(parse(text = x, keep.source = TRUE)))
  return(sol)
}

# Get all strings for get all strings for the expressions that correspond to a certain function
# the parse data passed should be for a single call!!!
get_expressions_for_function_call = function(fun, pd) {
  if(is.null(fun))
    return(getParseText(pd, id = max(pd$id)))
  ids_of_function_call = pd$id[pd$text == fun & pd$token != "SYMBOL"]
  return(sapply(ids_of_function_call, get_expression, pd))
}

# get the expression linked to an id in the parseData table
get_expression <- function(id, pd) {
  grandparent_id = pd$parent[pd$id == pd$parent[pd$id == id]]
  return(getParseText(pd, id = grandparent_id))
}

# From all parsedata for a set of commands, return only the parse data linked to a single command.
get_single_pd = function(index, pd, incorrect_number_of_calls_msg = NULL) {
  ok <- testwhat:::test_sufficient_length(stud = pd, index = index, incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  if(ok) {
    return(pd[[index]])
  } else {
    return(NULL)
  }
}

test_sufficient_length = function(stud, index, incorrect_number_of_calls_msg = NULL) {
  if(index < 1) {
    stop("The index argument must be positive!")
  }
  
  if(is.null(incorrect_number_of_calls_msg)) {
    incorrect_number_of_calls_msg <- sprintf("The system wants to test if the %s command you entered is correct, but it hasn't found one. Add more code.", get_num(index))
  }
  
  
  n_student_calls <- length(stud)
  sufficient_length <- (index <= n_student_calls)
  test_what(expect_true(sufficient_length), incorrect_number_of_calls_msg)
  
  return(sufficient_length)
}

# get all properties (uses ggvis function ggvis:::props!)
get_all_props = function(fun, expression) {
  extractor <- function(data, ...) {
    return(ggvis:::props(...))
  }
  
  expression = gsub(fun, "extractor", expression)
  out = try(eval(parse(text = expression)))
  if(inherits(out, "try-error")) {
    return(NULL)
  }
  else {
    # tidy up names and return
    names(out) = gsub(".update","",names(out))
    return(out)
  }
}