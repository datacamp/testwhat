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
    ex_type = "character",
    results = "list",
    silent = "numeric",
    silent_fail = "logical",
    instruction_index = "numeric",
    feedback = "list",
    success_msg = "character",
    tags = "list"),

  methods = list(
    ### overriden methods from Reporter
    start_reporter = function(...) {
      callSuper(...)
      results <<- list()
      silent <<- 0
      instruction_index <<- 0
      success_msg <<- sample(c("Good Job!", 
                               "Well done!", 
                               "Great work!"), 1)
      feedback <<- list()
    },

    set_data = function(feedback) {
      feedback <<- feedback
    },
    
    set_success_msg = function(msg = "") {
      success_msg <<- msg
    },
    
    add_result = function(result) {
      if(silent) {
        if(!result$passed) {
          silent_fail[silent] <<- TRUE
        }
      } else {
        results <<- c(results, list(list(passed = result$passed,
                                         feedback = feedback,
                                         instruction_index = instruction_index)))
        if(!result$passed) {
          failed <<- TRUE
        }
      }
    },

    ### new methods
    be_silent = function() {
      silent <<- silent + 1
      silent_fail[silent] <<- FALSE
    },

    be_loud = function() {
      silent <<- max(0, silent - 1)
    },
    
    get_silent_fail = function() {
      return(silent_fail[silent])
    },
    
    ## challenge methods
    set_instruction_index = function(index) {
      failed <<- FALSE
      instruction_index <<- index
    },
    
    get_feedback = function() {
      if (ex_type == "ChallengeExercise") {
        if(length(results) == 0) {
          stop("No tests written for challenge!")
        }
        
        instruction_indices <- unique(select_info(results, "instruction_index"))
        n_inst <- length(instruction_indices)
        
        if(n_inst < 2) {
          stop("Make sure to have at least two instructions, i.e. 1 step and the goal.")
        }
        
        if(max(instruction_indices) != n_inst) {
          stop("Make sure to write at least one test for each challenge step.")
        }
        
        res_per_inst <- sapply(1:n_inst, function(x) {
          all(select_info(results, "passed")[select_info(results, "instruction_index") == x])
        })
        
        challenge_passed <- tail(res_per_inst, 1)
        passed_steps <- head(res_per_inst, n_inst - 1)
        if (challenge_passed) {
          passed_steps <- rep(TRUE, n_inst - 1)
        }
        
        return(list(correct = challenge_passed, 
                    message = to_html(ifelse(challenge_passed, success_msg, "try again.")), 
                    steps_correct = passed_steps))
      } else {
        test_results <- select_info(results, "passed")
        if(!all(test_results)) {
          selector <- which(!test_results)[1]
          return(c(list(correct = FALSE),
                   results[[selector]]$feedback))
        } else {
          return(list(correct = TRUE, 
                      message = to_html(success_msg)))
        }
      }
    }
  )
)


#' @importFrom markdown markdownToHTML
to_html <- function(x) {
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  gsub("<p>(.*?)</p>", "\\1", html) #remove <p> tags, coded by front end.
}

select_info <- function(x, col) {
  sapply(x, `[[`, col)
}