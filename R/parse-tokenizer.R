#' @importFrom R6 R6Class
RTokenizer <- R6::R6Class("RTokenizer",
  public = list(
    initialize = function(code) {
      private$data <- strsplit(code, split = "")[[1]]
      private$pos_df <- private$build_pos_df()
    },
    next_token = function() {
      if (private$eol()) {
        return(NULL)
      }
      current <- private$peek()
      switch(current,
             '_' = {
               if (!isTRUE(grepl("[a-zA-Z0-9]", private$peek(-1))) && 
                   isTRUE(private$peek(1) == '_') && 
                   isTRUE(private$peek(2) == "_")) {
                 private$consume_token("FILL_IN", 3)  
               } else {
                 private$consume_token("OTHER", 1)
               }
             },
             '(' = private$consume_token("LPAREN", 1),
             ')' = private$consume_token("RPAREN", 1),
             '{' = private$consume_token("LBRACE", 1),
             '}' = private$consume_token("RBRACE", 1),
             '[' = {
               if (isTRUE(private$peek(1) == '[')) {
                 private$push_brace("LDBRACKET")
                 token <- private$consume_token("LDBRACKET", 2)
               } else {
                 private$push_brace("LBRACKET")
                 token <- private$consume_token("LBRACKET", 1)
               }
               token
             },
             ']' = {
               if (isTRUE(private$peek(1) == ']')) {
                 top <- tail(private$brace_stack, 1)
                 if (top == "LDBRACKET") {
                   token <- private$consume_token("RDBRACKET", 2)
                 } else {
                   token <- private$consume_token("RBRACKET", 1)
                 }
               } else {
                 token <- private$consume_token("RBRACKET", 1)
               }
               return(token)
             },
             '"' = private$match_string(),
             '\'' = private$match_string(),
             ' ' = private$match_whitespace(),
             '\t' = private$match_whitespace(),
             '\r' = private$match_whitespace(),
             '\n' = private$match_whitespace(),
             '#' = private$match_comment(),
             private$consume_token("OTHER", 1))
    }
  ),
  private = list(
    data = character(0),
    pos = 1,
    pos_df = NULL,
    build_pos_df = function() {
      n <- length(private$data)
      df <- data.frame(line = integer(length(n)), column = integer(length(n)))
      line <- 1
      column <- 1
      for (i in 1:(length(private$data) + 1)) {
        df[i, 'line'] <- line
        df[i, 'column'] <- column
        if (isTRUE(private$data[i] == "\n") || isTRUE(private$data[i] == "\r")) {
          column <- 1
          line <- line + 1
        } else {
          column <- column + 1
        }
      }
      df
    },
    eol = function() {
      return(private$pos > length(private$data))
    },
    peek = function(lookahead = 0) {
      loc <- private$pos + lookahead
      if (loc < 0 || loc > length(private$data)) {
        return(NA)
      } else {
        return(private$data[loc])  
      }
    },
    brace_stack = character(0),
    push_brace = function(el) {
      private$brace_stack <- c(private$brace_stack, el)
    },
    pop_brace = function(el) {
      n <- length(private$brace_stack)
      if (n == 0) {
        stop("Can't pop a brace!")
      }
      private$brace_stack <- brace_stack[-n]
    },
    consume_token = function(type, len) {
      if (len == 0 || private$pos - 1 + len > length(private$data)) {
        return(NULL)
      }
      # -1 hack to make it work in the front end (front end is 'up to and including')
      token <- RToken$new(type = type, 
                          line_start = private$pos_df[private$pos, 'line'],
                          column_start = private$pos_df[private$pos, 'column'],
                          line_end = private$pos_df[private$pos + len , 'line'],
                          column_end = private$pos_df[private$pos + len, 'column'] - 1)
      private$pos <- private$pos + len
      return(token)
    },
    match_string = function() {
      start <- private$pos
      # borrow implementation from rstudio
      quot <- private$eat()
      
      while (!private$eol()) {
        hit <- private$eat_until("[\'\"]")
        
        if (!hit) {
          break
        }
        
        c <- private$eat()
        if (c == quot) {
          # -1 hack to make it work in the front end (front end is 'up to and including')
          token <- RToken$new(type = "STRING", 
                              line_start = private$pos_df[start, 'line'],
                              column_start = private$pos_df[start, 'column'],
                              line_end = private$pos_df[private$pos - 1, 'line'],
                              column_end = private$pos_df[private$pos, 'column'] - 1)
          return(token)
        }
      }
      line <- private$pos_df[start, 'line']
      column <- private$pos_df[start, 'column']
      token <- RToken$new(type = "INCOMPLETE_STRING", 
                          line_start = line,
                          column_start = column,
                          line_end = line,
                          column_end = column)
      return(token)
    },
    match_whitespace = function() {
      private$consume_token("WHITESPACE", len = private$get_match_length("\\s+"))
    },
    match_comment = function() {
      private$consume_token("COMMENT", len = private$get_match_length("#[^\n]*"))
    },
    get_match_length = function(patt) {
      the_str <- paste(private$data[private$pos:length(private$data)], collapse = "")
      matches <- gregexpr(patt, the_str)[[1]]
      if (any(matches == -1)) {
        stop("Something went wrong in looking for that pattern...")
      }
      if (matches[1] != 1) {
        stop("Pattern match should be at the beginning...")
      }
      attr(matches, "match.length")[1]
    },
    eat = function() {
      char <- private$data[private$pos]
      private$pos <- private$pos + 1
      return(char)
    },
    eat_until = function(patt) {
      the_str <- paste(private$data[private$pos:length(private$data)], collapse = "")
      matches <- gregexpr(patt, the_str)[[1]]
      if (any(matches == -1)) {
        # eat all because no match found
        private$pos <- length(private$data)
        return(FALSE)
      } else {
        private$pos <- private$pos - 1 + matches[1]
        return(TRUE)
      }
    }
  )
)