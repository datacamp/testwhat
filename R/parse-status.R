#' @importFrom R6 R6Class
ParseStatus <- R6::R6Class("ParseStatus", 
  public = list(
    initialize = function() {},
#     push_state = function(state) {
#       private$states <- c(private$states, list(state))
#     },
#     pop_state = function() {
#       n <- length(private$states)
#       if (n == 0) stop("no more states")
#       tail <- private$states[[n]]
#       private$states <- private$states[-n]
#       tail
#     },
    push_bracket = function(token) {
      private$bracket_stack <- c(private$bracket_stack, list(token))
    },
    pop_bracket = function(rhs) {
      n <- length(private$bracket_stack)
      if (n == 0) {
        self$add_lint(rhs, sprintf("This %s wasn't expected.", human_name(rhs$type)))
        return(NULL)
      } 
      
      lhs <- private$bracket_stack[[n]]
      if (complement(lhs$type) != rhs$type) {
        self$add_lint(lhs, sprintf("Make sure to correctly close this %s.", human_name(lhs$type)))
        return(NULL)
      }
  
      private$bracket_stack <- private$bracket_stack[-n]
    },

    add_lint = function(token, message) {
      private$lints <- c(private$lints, 
                         list(c(token$location, message = paste(fail_msg, message))))
    },

    lint_present = function() {
      length(private$lints) > 0
    },

    get_lint = function(index = 1) {
      private$lints[[index]]
    },

    get_finish_lint = function() {
      n <- length(private$bracket_stack)
      if (n > 0) {
        token <- private$bracket_stack[[n]]
        return(c(token$location, list(message = paste(fail_msg, sprintf("Don't forget to close this %s.", human_name(token$type))))))
      } else {
        return(list(message = parse_fallback_msg))
      }
    }
  ),
  private = list(
    states = list(),
    bracket_stack = list(),
    lints = list()
  )
)