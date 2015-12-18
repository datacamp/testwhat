#' @export
test_ggplot <- function(index = 1,
                        student_code = get_student_code(),
                        solution_code = get_solution_code(),
                        student_env = .GlobalEnv,
                        solution_env = get_solution_env()) {
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
  
  feedback <- sprintf("For ggplot command %s,", index)
  
  stud_selected <- student_ggplot_objects[[index]]
  
  # Check the data
  sol_data <- list(base = sol_selected$data)
  
  stud_data <- list(base = stud_selected$data)

  test_what(expect_equal(sol_data$base, stud_data$base), feedback_msg = paste(feedback, "you didn't get the data layer correct."))
  
  # Check the mapping
  sol_mapping <- list(base = sol_selected$mapping)

  stud_mapping <- list(base = stud_selected$mapping)
  
  for (map in names(sol_mapping$base)) {
    test_what(expect_false(is.null(stud_mapping$base[map][[1]])), feedback_msg = paste0(feedback, " have you mapped something on the `", map, "` aesthetic?"))
    test_what(expect_equal(stud_mapping$base[map], sol_mapping$base[map]), feedback_msg = paste0(feedback, " have you mapped `", sol_mapping$base[map] ,"` on the `", map, "` aesthetic? Found `", stud_mapping$base[map], "`."))
  }
  
  # Check the geom layer
  stud_layers <- stud_selected$layers
  
  for (sol_layer in sol_selected$layers) {
    found_geom_name <- FALSE
    found_geom_with_params <- TRUE
    
    sol_params <- sol_layer$geom_params
    sol_params <- c(sol_params, sol_layer$stat_params)
    sol_params <- c(sol_params, lapply(sol_layer$mapping, function(x) structure(x, aes = TRUE)))
    sol_params_failed <- c()
    
    nb_stud_layers <- length(stud_layers)
    if (nb_stud_layers > 0) {
      for (i in 1:nb_stud_layers) {
        stud_layer <- stud_layers[[i]]
        if (stud_layer$geom$objname == sol_layer$geom$objname) {
          found_geom_name <- TRUE
          found_geom_with_params <- TRUE
          stud_params <- stud_layer$geom_params
          stud_params <- c(stud_params, stud_layer$stat_params)
          stud_params <- c(stud_params, lapply(stud_layer$mapping,function(x) structure(x, aes = TRUE)))
          
          for (sol_param in names(sol_params)) {
            if (!(sol_param %in% names(stud_params))) {
              found_geom_with_params <- FALSE
              sol_params_failed <- c(sol_params_failed, sol_param)
              break
            } else {
              sol_value <- sol_params[[sol_param]]
              stud_value <- stud_params[[sol_param]]
              
              if (!all(sol_value == stud_value)) {
                found_geom_with_params <- FALSE
                sol_params_failed <- c(sol_params_failed, sol_param)
                break
              }
            }
          }
          
          if (found_geom_with_params) {
            stud_layers[[i]] <- NULL
            break
          }
        }
      }
    }
    
    geom_base_feedback <- paste0(feedback, " have you correctly added a `geom_", sol_layer$geom$objname,"()` layer")
    test_what(expect_true(found_geom_name), feedback_msg = paste0(geom_base_feedback, "?"))
    param_feedback <- paste(vapply(unique(sol_params_failed), function(x) paste0(ifelse(isTRUE(attr(sol_params[[x]], "aes")), "aesthetic ", ""), "`", x, "` set to `", sol_params[[x]], "`"), character(1)), collapse = ", ")
    test_what(expect_true(found_geom_with_params), feedback_msg = paste0(geom_base_feedback, " with ", param_feedback, "?"))
  }
}

get_ggplot_objects <- function(code, envir) {
  return(lapply(get_ggplot_commands(code), function(x) eval(x, envir)))
}

get_ggplot_commands <- function(code) {
  parsed <- try(parse(text = code), silent = TRUE)
  
  if (inherits(parsed, "try-error")) {
    return(list())
  }
  
  extracted <- lapply(parsed, extract_ggplot_command)
  return(extracted[!as.logical(vapply(extracted, is.null, logical(1)))])
}

extract_ggplot_command <- function(code) {
  if (is_ggplot_command(code)) {
    return(code)
  } else if (is.name(code)) {
    return(NULL)
  } else if (code[[1]] == "=" || code[[1]] == "<-") {
    return(extract_ggplot_command(code[[3]]))
  } else {
    return(NULL)
  }
}

is_ggplot_command <- function(code) {
  if (!is.call(code)) {
    return(FALSE)
  } else if (code[[1]] == "ggplot") {
    return(TRUE)
  } else if (code[[1]] == "+") {
    return(is_ggplot_command(code[[2]]))
  } else {
    return(FALSE)
  }
}