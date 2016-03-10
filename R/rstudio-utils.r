create_student_pd <- function(student_code = NULL) {
  student_pd <- tw$get("student_pd")
  if(!is.null(student_pd)) return(student_pd)
  
  if(is.null(student_code)) {
    stop("The student_code argument cannot be empty")
  }
  student_code_parts = get_clean_lines(code = student_code)
  stud = lapply(student_code_parts, function(x) getParseData(parse(text = x, keep.source = TRUE)))
  tw$set(student_pd = stud)
  return(stud)
}

create_solution_pd <- function(solution_code = NULL) {
  solution_pd <- tw$get("solution_pd")
  if(!is.null(solution_pd)) return(solution_pd)
  
  if(is.null(solution_code)) {
    stop("The solution_code argument cannot be empty")
  }
  solution_code_parts = get_clean_lines(code = solution_code)
  sol = lapply(solution_code_parts, function(x) getParseData(parse(text = x, keep.source = TRUE)))
  tw$set(solution_pd = sol)
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

## GGVIS

# get all properties (uses ggvis function ggvis:::props!)
get_all_props = function(fun, expression) {
  extractor <- function(ignore, ...) {
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