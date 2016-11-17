#' @importFrom R6 R6Class
RToken <- R6::R6Class("RToken", 
  public = list(
    type = character(0),
    location = list(),
    initialize = function(type, line_start, column_start, line_end, column_end) {
      self$type = type
      self$location = list(line_start = line_start,
                           column_start = column_start, 
                           line_end = line_end, 
                           column_end = column_end)
    }
  )
)

#' @importFrom R6 R6Class
RTokenCursor <- R6::R6Class("RTokenCursor",
  public = list(
    tokens = NULL,
    initialize = function(code) {
      tokenizer <- RTokenizer$new(code)
      token <- tokenizer$next_token()
      tokens <- list()
      while (!is.null(token)) {
        # do away with comments
        if (token$type != "COMMENT") {
          tokens <- c(tokens, list(token))
        }
        token <- tokenizer$next_token()
      }
      self$tokens <- tokens
      private$offset <- 1
      private$n <- length(tokens)
    },
    move_up = function() {
      if (self$is_at_eod()) {
        return(invisible(NULL))
      } else {
        private$offset <- private$offset + 1
        return(invisible(self$current_token()))
      }
    },
    move_to_next_token = function() {
      current <- self$move_up()
      while (current$type %in% c("OTHER", "WHITESPACE", "COMMENT", "STRING") && !self$is_at_eod()) {
        current <- self$move_up()
      }
    },
    is_at_eod = function() {
      private$offset == private$n
      # TODO make more complete (borrow from RTokenCursor::isAtEndOfDocument)
    },
    current_token = function() {
      self$tokens[[private$offset]]
    },
    is_type = function(types) {
      self$current_token()$type %in% types
    },
    get_type = function() {
      self$current_token()$type
    }
  ),
  private = list(
    offset = integer(0),
    n = integer(0)
  )
)