is_white_space_or_comment <- function(token) {
  token$type == "WHITESPACE" || token$type == "COMMENT"
}

complement <- function(type) {
  switch(type,
         LPAREN = "RPAREN",
         RPAREN = "LPAREN",
         LBRACE = "RBRACE",
         RBRACE = "LBRACE",
         LDBRACKET = "RDBRACKET",
         RDBRACKET = "LDBRACKET",
         LBRACKET = "RBRACKET",
         RBRACKET = "LBRACKET",
         "ERROR")
}

human_name <- function(type) {
  switch(type,
         LPAREN = "parenthesis",
         RPAREN = "parenthesis",
         LBRACE = "curly bracket",
         RBRACE = "curly bracket",
         LDBRACKET = "double bracket",
         RDBRACKET = "double bracket",
         LBRACKET = "bracket",
         RBRACKET = "bracket",
         "ERROR")
}

fail_msg <- "Have a look at the highlighted code in the editor."
parse_fallback_msg <- "Your code contains a syntax error. Check the console output and try to fix the issue."
