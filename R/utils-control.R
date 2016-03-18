extract_control <- function(pd, keyword, elnames) {
  if(any(pd$token == keyword)) {
    # Intersection of parent of ids with the correct keyword WITH
    # ids that are top-level (whose parent is not in pd) OR
    # ids of children of top-level parents that have curly brackets
    parents <- pd$parent[pd$token == keyword]
    top_level_ids <- pd$id[!(pd$parent %in% pd$id)]
    top_level_ids_with_curly_brackets <- pd$id[grepl("^\\s*\\{.*?\\}\\s*$", pd$text) & pd$id %in% top_level_ids]
    children_of_curly_brackets <- pd$id[pd$parent %in% top_level_ids_with_curly_brackets]
    ids <- base::intersect(parents, c(top_level_ids, children_of_curly_brackets))
                     
    chop_up_pd <- function(id, elnames) {
      expr_ids <- pd$id[pd$parent == id & pd$token %in% c("expr", "forcond")]
      sub_codes <- lapply(expr_ids, getParseText, parseData = pd)
      
      get_sub_pd <- function(id) {
        children <- get_children(pd, id)
        pd[pd$id %in% c(children, id), ]
      }
      
      sub_pds <- lapply(expr_ids, get_sub_pd)
      out <- mapply(function(code, pd) list(code = code, pd = pd), sub_codes, sub_pds, SIMPLIFY = FALSE)
      names(out) <- elnames[1:length(out)]
      out
    }
    
    lapply(ids, chop_up_pd, elnames = elnames)
  } else {
    return(list())
  }
}

extract_if <- function(pd) {
  extract_control(pd, keyword = "IF", elnames = c("cond_part", "if_part", "else_part"))
}

extract_for <- function(pd) {
  extract_control(pd, keyword = "FOR", elnames = c("cond_part", "expr_part"))
}

extract_while <- function(pd) {
  extract_control(pd, keyword = "WHILE", elnames = c("cond_part", "expr_part"))
}

prepare_tw <- function(stud, sol, part) {
  tw$set(student_pd = stud[[part]][["pd"]])
  tw$set(solution_pd = sol[[part]][["pd"]])
  tw$set(student_code = stud[[part]][["code"]])
  tw$set(solution_code = sol[[part]][["code"]])
  tw$set(fun_usage = NULL)
}



