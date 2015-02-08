#' DataCamp API overrides
#'
#' Functions that override the API functionality if it is not available.
#'
#' @name overrides
#' @keywords internal
#' @export
get_solution_env <- function() {
  if(api_available()) {
    return(datacampAPI::get_solution_env())
  } else {
    position <- which(search() == "env:SOL")
    if(length(position) == 0) {
      attach(new.env(), name = "env:SOL")
      position <- which(search() == "env:SOL")
    }
    as.environment(position)
  }
}

#' @rdname overrides
#' @export
get_student_code <- function() {
  if(api_available()) {
    return(datacampAPI::get_student_code())
  } else {
    get_variable("student_code")
  }
}

#' @rdname overrides
#' @export
set_student_code <- function(x) {
  if(api_available()) {
    datacampAPI::set_student_code(x)
  } else {
    assign("student_code",x,envir = globalenv())
  }
}

#' @rdname overrides
#' @export
get_solution_code <- function() {
  if(api_available()) {
    return(datacampAPI::get_solution_code())
  } else {
    get_variable("solution_code")
  }
}

#' @rdname overrides
#' @export
set_solution_code <- function(x) {
  if(api_available()) {
    datacampAPI::set_solution_code(x)
  } else {
    assign("solution_code",x,envir = globalenv())
  }
}

#' @rdname overrides
#' @export
get_student_output <- function() {
  if(api_available()) {
    return(datacampAPI::get_student_output())
  } else {
    get_variable("student_output")
  }
}

#' @rdname overrides
#' @export
set_student_output <- function(x) {
  if(api_available()) {
    datacampAPI::set_student_output(x)
  } else {
    assign("student_output",x,envir = globalenv())
  }
}

#' @rdname overrides
#' @export
get_student_error <- function() {
  if(api_available()) {
    return(datacampAPI::get_student_error())
  } else {
    get_variable("student_error")
  }
}

#' @rdname overrides
#' @export
set_student_error <- function(x) {
  if(api_available()) {
    datacampAPI::set_student_error(x)
  } else {
    assign("student_error",x,envir = globalenv())
  }
}

#' @rdname overrides
#' @export
get_sct_result <- function() {
  if(api_available()) {
    return(datacampAPI::get_sct_result())
  } else {
    get_variable("sct_result")
  }
}

#' @rdname overrides
#' @export
set_sct_result <- function(x) {
  if(api_available()) {
    datacampAPI::set_sct_result(x)
  } else {
    assign("sct_result",x,envir = globalenv())
  }
}

api_available <- function() requireNamespace("datacampAPI", quietly=TRUE)

get_variable = function(name, message = NULL, envir = globalenv()) {
  if(exists(name, envir = envir, inherits = FALSE)) {
    get(name, envir = envir, inherits = FALSE)
  } else {
    if(is.null(message)) {
      message <- paste(name, " is not available.")
    }
    stop(message)
  }
}
