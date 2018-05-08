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

