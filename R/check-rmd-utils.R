find_same_line <- function(lines, patt) {
  hits <- which(grepl(patt, lines))
  return(hits)
}

find_prev_line <- function(lines, patt) {
  candidates <- which(grepl(patt, lines)) - 1
  # previous row must contain code
  return(candidates[!grepl("^\\s*$", lines[candidates])])
}

get_header_hits <- function(lines, level) {
  all_hits <- list(
    h1 = sort(c(length(lines) + 1,
                find_same_line(patt = "^#\\s+.*?", lines = lines),
                find_prev_line(patt = "^={5,}$", lines = lines))),
    h2 = sort(c(length(lines) + 1,
                find_same_line(patt = "^##\\s+.*?", lines = lines),
                find_prev_line(patt = "^-{5,}$", lines = lines))),
    h3 = sort(c(length(lines) + 1, find_same_line(patt = "^###\\s+.*?", lines = lines)))
  )
  
  hits <- all_hits[[paste0('h', level)]]
  if(is.null(hits)) stop(sprintf("No pattern matching available for level %i.", level))
  return(hits)
}

get_header_elements <- function(lines, hits, index) {
  title <- lines[hits[index]]
  contents <- lines[(hits[index] + 1): (hits[index+1] - 1)]
  contents <- contents[!grepl("^-{5,}$", contents)]
  contents <- contents[!grepl("^={5,}$", contents)]
  contents <- paste0(contents, collapse = "\n")
  
  return(list(contents = contents, title = title))
}

get_chunks <- function(code, index = index) {
  doc_structure <- build_doc_structure(code)
  chunks <- doc_structure[sapply(doc_structure, class) == "block"]
  return(chunks)
}

select_chunk <- function(chunks, index) {
  payload = list(params = chunks[[index]]$params,
                 code = chunks[[index]]$input,
                 pd = build_pd(chunks[[index]]$input))
  return(payload)
}

parse_yaml <- function(code) {
  rmarkdown:::parse_yaml_front_matter(strsplit(code, split = "\n")[[1]])
}


check_equal_option_helper <- function(state, type, incorrect_msg, append) {
  student_option <- state$get("student_option")
  solution_option <- state$get("solution_option")
  state$add_details(type = type,
                    case = "equal",
                    student = student_option,
                    solution = solution_option,
                    message = incorrect_msg,
                    append = append)
  
  check_that(is_equal(student_option, solution_option, "equal"),
             feedback = state$details)
  
  return(state)
}