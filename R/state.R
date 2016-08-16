#' State object
#'
#' @export
#' @importFrom R6 R6Class
State <- R6::R6Class("State",
  public = list(
    set = function(...) {
      els <- list(...)
      for (i in seq_along(els)) {
       tryCatch(assign(names(els)[i], els[[i]], envir = private),
                error = function(e) {
                  stop(sprintf("Cannot set '%s'; the name is invalid.", names(els[i])))
                })
      }
    },
    
    # blacklisting stuff
    update_blacklist = function() {
      # first, blacklist earlier work, if any.
      fun_usage <- private$fun_usage
      l <- length(fun_usage)
      if (l > 0) {
        if (l == 1) {
          df <- as.data.frame(fun_usage)  
        } else {
          df <- do.call(rbind.data.frame, c(fun_usage, list(stringsAsFactors = FALSE)))
        }
        agg <- aggregate(df$success, by=list(stud_index = df$stud_index), FUN=all)
        passed_stud_indices <- agg$stud_index[agg$x]
        if (length(passed_stud_indices) > 0) {
          ind_to_blacklist <- min(passed_stud_indices)
          stopifnot(length(unique(df$name)) == 1)
          stopifnot(length(unique(df$sol_index)) == 1)
          private$set_used(df$name[1], df$sol_index[1], ind_to_blacklist) 
        }
      }
      private$fun_usage <- list()
    },
    
    log = function(index, arg = NULL, success) {
      if (is.null(private$fun_usage)) {
        # fun usage not defined at this level, throw up
        private$parent$log(index, arg = arg, success)
      } else {
        if (!is.null(arg)) {
          private$active_arg <- arg
        }
        private$fun_usage <- c(private$fun_usage,
                               list(list(name = private$active_name,
                                         sol_index = private$active_sol_index,
                                         arg = private$active_arg,
                                         stud_index = index,
                                         success = success)))  
      }
    },
    
    get_options = function(n_calls) {
      if (is.null(private$active_name) || 
          is.null(private$active_sol_index) ||
          is.null(private$blacklist)) {
        # required info not at this level, throw up
        self$parent$get_options()
      }
      name = private$active_name
      sol_index = private$active_sol_index
      bl <- private$blacklist
      name_hits <- sapply(bl, `[[`, "name") == name
      bl <- bl[name_hits]
      sol_index_hits <- sapply(bl, `[[`, "sol_index") == sol_index
      if (any(sol_index_hits)) {
        bl <- bl[sol_index_hits]
        bl[[1]]$stud_index
      } else {
        setdiff(1:n_calls, sapply(bl, `[[`, "stud_index"))
      }
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
    
    # fun usage
    fun_usage = NULL,
    active_name = NULL,
    active_sol_index = NULL,
    active_arg = NULL,
    blacklist = list(),
    set_used = function(name, sol_index, stud_index) {
      private$blacklist = c(private$blacklist, 
                            list(list(name = name, 
                                      stud_index = stud_index, 
                                      sol_index = sol_index)))
    }
  )
)

# State that has no parent
RootState <- R6::R6Class("RootState", inherit = State,
  public = list(
    initialize = function(...) {
      self$set(...)
    },
    
    get = function(name) {
      return(private[[name]])
    }
  )
)
                         
ChildState <- R6::R6Class("ChildState", inherit = State,
  public = list(
    
    initialize = function(state) {
      private$parent = state
      # copy details from parent
      self$details = private$parent$details
    },
    
    get = function(name) {
      el <- private[[name]]
      if (is.null(el)) {
        el <- private$parent$get(name)
      }
      return(el)
    },
    
    add_details = function(...) {
      self$details <- c(self$details, list(list(...)))
    },
    
    set_details = function(...) {
      det <- list(...)
      n <- length(self$details)
      self$details[[n]][names(det)] <- det
    },
    
    details = list()
  ),
  
  private = list(
    parent = NULL
  )
)

FunctionState <- R6::R6Class("FunctionState", inherit = ChildState,
                             private = list(student_calls = NULL,
                                            solution_call = NULL))

OperationState <- R6::R6Class("OperationState", inherit = ChildState,
                              private = list(student_ops = NULL,
                                             solution_op = NULL))


ArgumentState <- R6::R6Class("ArgumentState", 
                             inherit = ChildState,
                             private = list(student_args = NULL,
                                            solution_arg = NULL))

ObjectState <- R6::R6Class("ObjectState", inherit = ChildState,
                           private = list(name = NULL,
                                          student_object = NULL,
                                          solution_object = NULL))

FunDefState <- R6::R6Class("FunDefState", inherit = ChildState,
                           private = list(student_object = NULL,
                                          solution_object = NULL))

FunDefArgsState <- R6::R6Class("FunDefArgsState", inherit = FunDefState)
FunDefResultState <- R6::R6Class("FunDefResultState", inherit = FunDefState)
FunDefOutputState <- R6::R6Class("FunDefOutputState", inherit = FunDefState)
FunDefErrorState <- R6::R6Class("FunDefErrorState", inherit = FunDefState)
  
ControlState <- R6::R6Class("ControlState",
                            inherit = ChildState,
                            private = list(student_struct = NULL,
                                           solution_struct = NULL))

SubState <- R6::R6Class("SubState", inherit = ChildState)

RegexState <- R6::R6Class("RegexState", inherit = ChildState)
FileState <- R6::R6Class("FileState", inherit = ChildState)
ErrorState <- R6::R6Class("ErrorState", inherit = ChildState)

MarkdownState <- R6::R6Class("MarkdownState", inherit = ChildState,
                             private = list(student_ds = NULL,
                                            solution_ds = NULL,
                                            student_ds_part = NULL,
                                            solution_ds_part = NULL,
                                            inline_number = NULL,
                                            chunk_number = NULL))
ex <- function() {
  return(tw$get("state"))
}
