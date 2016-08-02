#' State object
#'
#' @export
#' @importFrom R6 R6Class
State <- R6::R6Class("State",
  public = list(
     
    initialize = function(...) {
      self$set(...)
    },
    
    get = function(name) {
      return(private[[name]])
    },
    
    set = function(...) {
      els <- list(...)
      for (i in seq_along(els)) {
       tryCatch(assign(names(els)[i], els[[i]], envir = private),
                error = function(e) {
                  stop(sprintf("Cannot set '%s'; the name is invalid.", names(els[i])))
                })
      }
    },
    
    get_root = function() {
     return(self)
    }
  ),
   
  private = list(
    pec = NULL,
    student_code = NULL,
    student_pd = NULL,
    student_env = NULL,
    solution_code = NULL,
    solution_pd = NULL,
    solution_env = NULL,
    output_list = NULL,
    test_env = NULL,
    fun_usage = NULL,
    details = list()
  )
)

ChildState <- R6::R6Class("ChildState", inherit = State,
  public = list(
    
    initialize = function(state) {
      private$parent = state
      private$details = state$get("details")
    },
    
    get_root = function() {
      return(private$parent$get_root())
    },
    
    get = function(name) {
      el <- private[[name]]
      if (is.null(el)) {
        el <- private$parent$get(name)
      }
      return(el)
    },
    
    add_details = function(...) {
      private$details <- c(private$details, list(list(...)))
    },
    
    set_details = function(...) {
      det <- list(...)
      n <- length(private$details)
      private$details[[n]][names(det)] <- det
    }
  ),
  
  private = list(
    parent = NULL,
    details = NULL
  )
)

FunctionState <- R6::R6Class("FunctionState", inherit = ChildState,
  public = list(),
  private = list(
    fun_name = NULL,
    student_calls = NULL,
    solution_call = NULL
))

ObjectState <- R6::R6Class("ObjectState", inherit = ChildState,
  public = list(),
  private = list(
    object_name = NULL,
    student_object = NULL,
    solution_object = NULL
  )
)

ex <- function() {
  return(tw$get("state"))
}
