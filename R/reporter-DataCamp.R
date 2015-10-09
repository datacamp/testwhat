setOldClass('proc_time')

#' DataCamp reporter: gather test results along with elapsed time and
#' feedback messages.
#'
#' This reporter gathers all results, adding additional information such as
#' test elapsed time and feedback messages.
#'
#' @export
#' @export DataCampReporter
#' @aliases DataCampReporter
#' @keywords debugging
DataCampReporter <- setRefClass(
  "DataCampReporter", contains = "Reporter",
  fields = list(
    report = "character",
    results = "list",
    silent = "logical",
    silent_fail = "logical",
    instruction_index = "numeric",
    inh_failure_msg = "character",
    inh_success_msg = "character"),

  methods = list(
    ### overriden methods from Reporter
    initialize = function(...) {
      report <<- "first"
      callSuper(...)
      
    },
    start_reporter = function(...) {
      callSuper(...)
      results <<- list()
      silent <<- FALSE
      instruction_index <<- 0
      inh_failure_msg <<- ""
      inh_success_msg <<- sample(c("You rock!", "You are a coding rockstar!", "Keep up the good work.", "Great job!", "Woot!", "Way to go!", "Nice code."), 1)
    },

    set_inh_failure_msg = function(msg = "") {
      inh_failure_msg <<- msg
    },
    
    clear_inh_failure_msg = function() {
      inh_failure_msg <<- ""
    },
    
    set_inh_success_msg = function(msg = "") {
      inh_success_msg <<- msg
    },
    
    add_result = function(result) {
      if(nchar(inh_failure_msg) == 0) {
        stop("No failure message defined. Use test_what around expect_ function.")
      }
      if(silent) {
        if(!result$passed) {
          silent_fail <<- TRUE
        }
      } else {
        results <<- c(results, list(list(passed = result$passed, 
                                         success_msg = result$success_msg,
                                         failure_msg = inh_failure_msg, 
                                         instruction_index = instruction_index)))
        if(!result$passed) {
          failed <<- TRUE
        }
      }
    },

    ### new methods
    be_silent = function() {
      silent <<- TRUE
      silent_fail <<- FALSE
    },

    be_loud = function() {
      silent <<- FALSE
    },
    
    ## challenge methods
    set_instruction_index = function(index) {
      failed <<- FALSE
      instruction_index <<- index
    },
    
    get_feedback = function() {
      results_df <- do.call("rbind", lapply(results, data.frame, stringsAsFactors = FALSE))
      if (report == "first") {
        test_results <- results_df$passed
        if(!all(test_results)) {
          feedback <- results_df$failure_msg[which(!test_results)[1]]
          return(list(passed = FALSE, feedback = feedback))
        } else {
          return(list(passed = TRUE, feedback = inh_success_msg))
        }
      } else if (report == "challenge") {
        
        if(length(results_df) == 0) {
          stop("No tests written for challenge!")
        }
        
        instruction_indices <- unique(results_df$instruction_index)
        n_inst <- length(unique(instruction_indices))
        
        if(n_inst < 2) {
          stop("Make sure to have at least two instructions, i.e. 1 step and the goal.")
        }
        
        if(max(instruction_indices) != n_inst) {
          stop("Make sure to write at least one test for each challenge step!")
        }
        
        res_per_inst <- sapply(1:n_inst, function(x) {
          all(results_df$passed[results_df$instruction_index == x])
        })
        
        challenge_passed <- tail(res_per_inst, 1)
        passed_steps <- head(res_per_inst, n_inst - 1)
        if (challenge_passed) {
          passed_steps <- rep(TRUE, n_inst - 1)
        }
        
        return(list(passed = challenge_passed, 
                    feedback = ifelse(challenge_passed, inh_success_msg, "try again."), 
                    passed_steps = passed_steps))
      } else {
        stop("Unknown report type!")
      }
    }
  )
)