do_parse <- function(code) {
  cursor <- RTokenCursor$new(code)
  status <- ParseStatus$new()
  
  if (is.null(cursor$tokens)) {
    return(status$get_finish_lint())
  }
  
  start <- function() {
  
    if (cursor$is_type("INCOMPLETE_STRING")) {
      status$add_lint(cursor$current_token(), "Make sure to close the string again!")  
    }
    
    if (cursor$is_type("FILL_IN")) {
      status$add_lint(cursor$current_token(), "Replace it with valid R code!")
    }
    
    if (cursor$is_type(c("LPAREN", "LBRACE", "LBRACKET", "LDBRACKET"))) {
      status$push_bracket(cursor$current_token())
      # type <- cursor$get_type()
      # state <- paste0("WITHIN_", substr(type, 2, nchar(type)), "S")
      # status$push_state(state)
    }
    
    if (cursor$is_type(c("RPAREN", "RBRACKET", "RDBRACKET", "RBRACE"))) {
      # Leave out the state stuff for now
      status$pop_bracket(cursor$current_token())
      # status$pop_state()
    }
  
    if (status$lint_present()) {
      return(status$get_lint())
    }
    
    if (cursor$is_at_eod()) {
      return(status$get_finish_lint())
    }
    
    cursor$move_to_next_token()
    start()
  }
  
  start()
}

#' Parse Rcpp code 
#' 
#' Parses Rcpp code (currently just the R chunks) and updates the state.
#' @param state An exercise state, as returned by \code{ex()}.
#' @return A child state.
#' @details The function extracts the R code lines from the state then parses
#' them. Currently the C++ code lines are not parsed, so they can only be 
#' checked using \code{check_code()}-based SCTs.
#' @export
parse_rcpp <- function(state) {
  childState <- ChildState$new(state)
  childState$set(
    student_code = extract_r_code_from_rcpp(state$get("student_code")),
    solution_code = extract_r_code_from_rcpp(state$get("solution_code"))
  )
  childState$set(
    student_pd = build_pd(childState$get("student_code")),
    solution_pd = build_pd(childState$get("solution_code"))
  )
  childState
}

seq_int <- function(lo, hi) {
  if(hi < lo) return(integer())
  seq.int(lo, hi, by = 1)
}

extract_r_code_from_rcpp <- function(code_lines, flatten = TRUE) {
  start_line <- which(grepl(" */\\*{3} +R", code_lines))
  end_line <- which(grepl(" *\\*/", code_lines))
  r_chunks <- Map(seq_int, start_line + 1, end_line - 1) %>% 
    lapply(function(x) code_lines[x])
  if(flatten) {
    r_chunks <- unlist(r_chunks, use.names = FALSE)
  }
  r_chunks
}
