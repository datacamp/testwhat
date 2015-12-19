#' @export
test_ggplot <- function(index = 1,
                        student_code = get_student_code(),
                        solution_code = get_solution_code(),
                        student_env = .GlobalEnv,
                        solution_env = get_solution_env(),
                        check_data = TRUE, data_fail_msg = NULL,
                        check_aes = TRUE, aes_fail_msg = NULL, exact_aes = FALSE,
                        check_geom = TRUE, geom_fail_msg = NULL, exact_geom = FALSE) {
  solution_ggplot_objects <- get_ggplot_objects(solution_code, solution_env)
  
  sol_selected <- try(solution_ggplot_objects[[index]], silent = TRUE)
  if (inherits(sol_selected, "try-error")) {
    stop(sprintf("Could not find ggplot command %d in your solution environment.", index))
  }
  
  student_ggplot_objects <- get_ggplot_objects(student_code, student_env)
  len <- length(student_ggplot_objects)
  
  if (len < index) {
    test_what(fail(), feedback_msg = "You didn't define enough `ggplot` commands.")
  }
  
  feedback <- sprintf("In your %s `ggplot` command,", nd(index))
  
  stud_selected <- student_ggplot_objects[[index]]
  
  test_what(expect_false(inherits(stud_selected, "try-error")), feedback_msg = paste(feedback, "you got an error. Make sure you use the correct `ggplot` syntax. Have another look at the instructions."))
  
  if (check_data) {
    # Check the data
    sol_data <- list(base = sol_selected$data)
    
    stud_data <- list(base = stud_selected$data)
  
    if (!is.null(data_fail_msg)) {
      feedback_msg <- data_fail_msg
    } else {
      feedback_msg <- paste(feedback, "you didn't get the data layer right.")
    }
    
    test_what(expect_equal(sol_data$base, stud_data$base), feedback_msg = feedback_msg)
  }
  
  if (check_aes) {
    # Check the mapping
    sol_mapping <- list(base = sol_selected$mapping)
  
    stud_mapping <- list(base = stud_selected$mapping)
    
    for (map in names(sol_mapping$base)) {
      if (!is.null(aes_fail_msg)) {
        feedback_msg <- rep_len(data_fail_msg, 3)
      } else {
        feedback_msg <- c(paste0(feedback, " have you mapped something on the `", map, "` aesthetic?"),
                          paste0(feedback, " have you mapped `", sol_mapping$base[map] ,"` on the `", map, "` aesthetic? Instead, you got `", stud_mapping$base[map], "`."),
                          paste0(feedback, " have you mapped exactly what is asked on the aesthetics layer, no more and no less?"))
      }
      
      test_what(expect_false(is.null(stud_mapping$base[map][[1]])), feedback_msg = feedback_msg[1])
      test_what(expect_equal(stud_mapping$base[map], sol_mapping$base[map]), feedback_msg = feedback_msg[2])
      if (exact_aes) {
        test_what(expect_equal(length(stud_mapping), length(sol_mapping)), feedback_msg = feedback_msg[3])
      }
    }
    
    
  }
  
  if (check_geom) {
    # Check the geom layer
    sol_layers <- sol_selected$layers
    stud_layers <- stud_selected$layers
    
    nb_sol_layers <- length(sol_layers)
    
    exact_geom <- rep_len(exact_geom, nb_sol_layers)
    
    for (i in 1:nb_sol_layers) {
      sol_layer <- sol_layers[[i]]
      
      found_geom_name <- FALSE
      found_geom_with_params <- FALSE
      found_geom_with_exact_params <- FALSE
      
      sol_params <- sol_layer$geom_params
      sol_params <- c(sol_params, sol_layer$stat_params)
      sol_params <- c(sol_params, lapply(sol_layer$mapping, function(x) structure(x, aes = TRUE)))
      
      nb_stud_layers <- length(stud_layers)
      if (nb_stud_layers > 0) {
        for (j in 1:nb_stud_layers) {
          stud_layer <- stud_layers[[j]]
          if (stud_layer$geom$objname == sol_layer$geom$objname) {
            found_geom_name <- TRUE
            found_params <- TRUE
            
            stud_params <- stud_layer$geom_params
            stud_params <- c(stud_params, stud_layer$stat_params)
            stud_params <- c(stud_params, lapply(stud_layer$mapping, function(x) structure(x, aes = TRUE)))
            
            for (sol_param in names(sol_params)) {
              if (!(sol_param %in% names(stud_params))) {
                found_params <- FALSE
                break
              } else {
                sol_value <- sol_params[[sol_param]]
                stud_value <- stud_params[[sol_param]]
                
                if (!all(sol_value == stud_value)) {
                  found_params <- FALSE
                  break
                }
              }
            }
            
            if (found_params) {
              found_geom_with_params <- TRUE
            }
            
            if (found_geom_with_params && (!exact_geom[i] || length(sol_params) == length(stud_params))) {
              found_geom_with_exact_params <- TRUE
            }
              
            if (found_geom_with_exact_params) {
              stud_layers[[j]] <- NULL
              break
            }
          }

        }
      }
      
      if (!is.null(geom_fail_msg)) {
        feedback_msg <- rep_len(geom_fail_msg, 3)
      } else {
        geom_base_feedback <- paste0(feedback, " have you correctly added a `geom_", sol_layer$geom$objname,"()` layer")
        filtered_geom_params <- names(filter_standard_geom_params(sol_layer$geom$objname, sol_params))
        param_strings <- vapply(filtered_geom_params, 
                                function(x) paste0(ifelse(isTRUE(attr(sol_params[[x]], "aes")), "aesthetic ", ""), 
                                                  "`", x, "` set to `", sol_params[[x]], "`"), character(1))
        nb_param_strings <- length(param_strings)
        if (nb_param_strings > 1) {
          param_feedback <- paste0(paste(param_strings[1:(nb_param_strings-1)], collapse = ", "), " and ", param_strings[nb_param_strings])
        } else {
          param_feedback <- param_strings
        }
        feedback_msg <- c(paste0(geom_base_feedback, " with a `+` operator?"),
                          paste0(geom_base_feedback, " with ", param_feedback, "?"),
                          paste0(geom_base_feedback, " with ", param_feedback, "?", " It seems like you have defined too much attributes or aesthetics for this geom."))
        
      }
        
      test_what(expect_true(found_geom_name), feedback_msg = feedback_msg[1])
      test_what(expect_true(found_geom_with_params), feedback_msg = feedback_msg[2])
      test_what(expect_true(found_geom_with_exact_params), feedback_msg = feedback_msg[3])
    }
  }
}

nd <- function(number) {
  switch(number, "1" = "first", "2" = "second", "3" = "third", 
                 "4" = "fourth", "5" = "fifth", "6" = "sixth", 
                 "7" = "seventh", "8" = "eighth", "9" = "ninth", 
                 "10" = "tenth")
}

filter_standard_geom_params <- function(geom_call, params) {
  standard_layer <- eval(call(paste0("geom_",geom_call)))
  standard_params <- standard_layer$geom_params
  standard_params <- c(standard_params, standard_layer$stat_params)
  standard_params <- c(standard_params, lapply(standard_layer$mapping,function(x) structure(x, aes = TRUE)))
  ov <- intersect(names(params), names(standard_params))
  eq <- mapply('==', standard_params[ov], params[ov])
  if (any(eq)) {
    params[[names(eq[eq])]] <- NULL
  } 
  return(params)
}

get_ggplot_objects <- function(code, envir) {
  ggplot_env <- new.env()
  return(lapply(get_ggplot_commands(code, ggplot_env), function(x) try(eval(x, envir), silent = TRUE) ))
}

get_ggplot_commands <- function(code, envir) {
  parsed <- try(parse(text = code), silent = TRUE)
  
  if (inherits(parsed, "try-error")) {
    return(list())
  }
  
  extracted <- lapply(parsed, extract_ggplot_command, envir = envir)
  return(extracted[!as.logical(vapply(extracted, is.null, logical(1)))])
}

extract_ggplot_command <- function(code, envir) {
  if (is_ggplot_command(code, envir)) {
    return(replace_saved_ggplot_commands(code, envir))
  } else if (is.name(code)) {
    return(NULL)
  } else if (code[[1]] == "=" || code[[1]] == "<-") {
    code_is_ggplot_command <- is_ggplot_command(code[[3]], envir)
    if (code_is_ggplot_command || is_geom_command(code[[3]], envir)) {
      extract_def <- replace_saved_ggplot_commands(code[[3]], envir)
      assign(as.character(code[[2]]), extract_def, envir = envir)
      if (isTRUE(code_is_ggplot_command)) {
        return(extract_def)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

replace_saved_ggplot_commands <- function(code, envir) {
  len <- length(code)
  if (len > 1) {
    for (i in 1:len) {
      if (exists(as.character(code[[i]]), envir = envir, inherits = FALSE)) {
        code[[i]] = get(as.character(code[[i]]), envir = envir, inherits = FALSE)  
      }
    }
  } else {
    if (exists(as.character(code), envir = envir, inherits = FALSE)) {
      code =  get(as.character(code), envir = envir, inherits = FALSE) 
    }
  }
  return(code)
}

is_ggplot_command <- function(code, envir) {
  if (is.name(code)) {
    get_command <- try(get(as.character(code), envir = envir, inherits = FALSE))
    return(ifelse(inherits(get_command, "try-error"), FALSE, is_ggplot_command(get_command, envir)))
  } else if (code[[1]] == "ggplot") {
    return(TRUE)
  } else if (code[[1]] == "+") {
    return(is_ggplot_command(code[[2]], envir))
  } else {
    return(FALSE)
  }
}

is_geom_command <- function(code, envir) {
  if (is.name(code)) {
    get_command <- try(get(as.character(code), envir = envir, inherits = FALSE))
    return(ifelse(inherits(get_command, "try-error"), FALSE, is_geom_command(get_command, envir)))
  } else {
    return(isTRUE(grepl("^geom_", code[[1]])))
  }
}