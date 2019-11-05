#' Check markdown file
#' 
#' Zoom in on contents of R Markdown code submission.
#' 
#' @param state the state to start from
#' 
#' @export
check_rmd <- function(state) {
  assert_state(state)
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")

  student_file <- names(student_code)[grepl(".rmd|.Rmd", names(student_code))]
  stopifnot(length(student_file) == 1)
  
  solution_file <- names(solution_code)[grepl(".rmd|.Rmd", names(solution_code))]
  stopifnot(length(solution_file) == 1)

  file_state <- SubState$new(state)
  
  file_state$set(student_code = unname(student_code[student_file]),
                 solution_code = unname(solution_code[solution_file]))
  return(file_state)
}

# Deprecated:
test_rmd_file <- function(code) {
  fail_if_v2_only()
  old_state <- ex()
  on.exit(tw$set(state = old_state))
  test_env <- old_state$get("test_env")
  tw$set(state = check_rmd(old_state))
  eval(substitute(code), envir = test_env)
}

#' Check markdown header
#'
#' Checks if a markdown header with a certain level exists. If not, generates a
#' feedback message. If yes, zooms in on the entire header section.
#'
#' @param state the state to start from.
#' @param level the level of the header to check
#' @param index which h<level> header to check
#' @param not_found_msg If specified, this overrides the automatically generated
#'   message in case not enough headers of the specified level were found.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states
#'
#' @return A state that zooms in on the section under the header until the next
#'   same-level header.
#'
#' @export
check_header <- function(state, level, index = 1, not_found_msg = NULL, append = TRUE) {
  assert_state(state)
  student_lines <- strsplit(state$get("student_code"), split = "\n")[[1]]
  solution_lines <- strsplit(state$get("solution_code"), split = "\n")[[1]]

  markdown_header_state <- MarkdownHeaderState$new(state)
  markdown_header_state$add_details(type = 'markdown_header',
                                    case = 'defined',
                                    index = index,
                                    level = level,
                                    message = not_found_msg,
                                    append = append,
                                    pd = NULL)
  
  student_header_hits <- get_header_hits(student_lines, level = level)
  solution_header_hits <- get_header_hits(solution_lines, level = level)
  
  if (index >= length(solution_header_hits)) {
    stop(sprintf("The solution doesn't contain %i headers of level %i itself.", index, level))
  }
  
  check_that(is_lt(index, length(student_header_hits)), feedback = markdown_header_state$details)
  
  student_header <- get_header_elements(student_lines, student_header_hits, index = index)
  solution_header <- get_header_elements(solution_lines, solution_header_hits, index = index)
  
  markdown_header_state$set_details(case = 'correct', message = NULL)
  markdown_header_state$set(student_code = student_header$contents,
                            solution_code = solution_header$contents,
                            student_title = student_header$title,
                            solution_title = solution_header$title)
  return(markdown_header_state)
}

#' Check markdown header title
#'
#' Checks if a title was specified for a markdown header. If not, generates a
#' feedback message. If yes, zooms in on the title so you can use
#' \code{check_equal}.
#'
#' @param state the state to start from. Should be a state produced by
#'   \code{\link{check_header}}.
#' @param not_found_msg If specified, this overrides the automatically generated
#'   message in case no title was specified.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states.
#'
#' @return A state that zooms in on the title of the header.
#'
#' @export
check_title <- function(state, not_found_msg = NULL, append = TRUE) {
  assert_state(state)
  stopifnot(inherits(state, "MarkdownHeaderState"))
  student_title <- state$get("student_title")
  solution_title <- state$get("solution_title")
  
  markdown_title_state <- ChildState$new(state)
  markdown_title_state$add_details(type = 'markdown_title',
                                   case = 'defined',
                                   message = not_found_msg,
                                   pd = NULL,
                                   append = append)
  
  if (is.null(solution_title) || solution_title == "character(0)") {
    stop("check_title couldn't retrieve the header title. Make sure you pass it a state coming from check_header.")
  }
  
  check_that(is_false(is.null(student_title) || student_title == "character(0)"),
             feedback = markdown_title_state$details)
  
  markdown_title_state$set_details(case = "correct", message = NULL, pd = NULL)
  markdown_title_state$set(student_code = student_title,
                           solution_code = solution_title)
  
  return(markdown_title_state)
}


#' Check markdown code chunk
#'
#' Checks if a code chunk was specified. If not, generates a feedback message.
#' If yes, zooms in on the code chunk so you can use functions like
#' \code{\link{check_function}} as for any regular R exercise.
#'
#' @param state the state to start from. This state can be produced by
#'   \code{\link{check_rmd}}, but can also follow on \code{\link{check_header}}
#'   to look for a chunk in a specific header section.
#' @param index number that specifies which code chunk to check in the student
#'   and solution code that is zoomed in on.
#' @param not_found_msg If specified, this overrides the automatically generated
#'   message in case no index'th chunk was found.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states.
#'
#' @return A state that zooms in on the code chunk.
#'
#' @export
check_chunk <- function(state, index = 1, not_found_msg = NULL, append = TRUE) {
  assert_state(state)
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
  student_chunks <- get_chunks(student_code)
  solution_chunks <- get_chunks(solution_code)
  
  markdown_chunk_state <- MarkdownChunkState$new(state)
  
  markdown_chunk_state$add_details(type = 'markdown_chunk',
                                   case = 'defined',
                                   index = index,
                                   message = not_found_msg,
                                   append = append,
                                   pd = NULL)
  
  if (index > length(solution_chunks)) {
    stop(sprintf("The solution doesn't contain %i code chunks in the piece of code that is zoomed in on.", index))
  }
  
  check_that(is_gte(length(student_chunks), index),
             feedback = markdown_chunk_state$details)
  
  student_chunk <- select_chunk(student_chunks, index = index)
  solution_chunk <- select_chunk(solution_chunks, index = index)
  markdown_chunk_state$set(student_code = student_chunk$code,
                           solution_code = solution_chunk$code,
                           student_pd = student_chunk$pd,
                           solution_pd = solution_chunk$pd,
                           student_options = student_chunk$params,
                           solution_options = solution_chunk$params)
  markdown_chunk_state$set_details(case = 'correct', message = NULL)
  return(markdown_chunk_state)
}

#' Check markdown code chunk option
#'
#' Checks if a code chunk option was specified. If not, generates a feedback
#' message. If yes, zooms in on the code chunk option so you can use
#' \code{\link{check_equal}} to verify equality.
#'
#' @param state the state to start from. Should be a state produced by
#'   \code{\link{check_chunk}}.
#' @param name name of the chunk option to zoom in on.
#' @param not_found_msg If specified, this overrides the automatically generated
#'   message in case the option wasn't specified.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states.
#' @param ... S3 stuff
#'
#' @return A state that zooms in on the code chunk option.
#'
#' @export
check_option.MarkdownChunkState <- function(state, name, not_found_msg = NULL, append = TRUE, ...) {
  assert_state(state)
  student_options <- state$get("student_options")
  solution_options <- state$get("solution_options")
  markdown_chunk_option_state <- MarkdownChunkOptionState$new(state)
  markdown_chunk_option_state$add_details(type = 'markdown_chunk_option',
                                          case = 'defined',
                                          name = name,
                                          message = not_found_msg,
                                          append = append,
                                          pd = NULL)
  if (!name %in% names(solution_options)) {
    stop(sprintf('check_option() tries to check the option %s of a code chunk, but it\'s not specified in the solution', name))
  }
  
  check_that(is_true(name %in% names(student_options)),
             feedback = markdown_chunk_option_state$details)
  
  markdown_chunk_option_state$set(student_option = student_options[[name]],
                                  solution_option = solution_options[[name]])
  markdown_chunk_option_state$set_details(case = 'correct', message = NULL)
  return(markdown_chunk_option_state)
}

#' Check equality of markdown code chunk option
#'
#' @param state the state to start from. Should be a state produced by \code{\link{check_option}}.
#' @param incorrect_msg If specified, this overrides the automatically generated message in case the options don't match between student and solution.
#' @param append Whether or not to append the feedback to feedback built in previous states.
#' @param ... S3 stuff
#'
#' @export
check_equal.MarkdownChunkOptionState <- function(state, incorrect_msg = NULL, append = FALSE, ...) {
  assert_state(state)
  check_equal_option_helper(state, "markdown_yaml_option", incorrect_msg = incorrect_msg, append = append)
}

#' Check markdown YAML header
#'
#' Checks if a YAML header was specified and can be parsed. If not, generates a
#' feedback message. If yes, parses the YAML header and zooms in on its options
#' so you can use \code{\link{check_option}} to verify the options.
#'
#' @param state the state to start from. This state should be produced by
#'   \code{\link{check_rmd}}.
#' @param error_msg If specified, this overrides the automatically generated
#'   message in case the YAML header couldn't be parsed.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states.
#'
#' @return A state that zooms in on the YAML header options.
#'
#' @export
check_yaml <- function(state, error_msg = NULL, append = TRUE) {
  assert_state(state)
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
  markdown_yaml_state <- MarkdownYamlState$new(state)
  
  markdown_yaml_state$add_details(type = 'markdown_yaml',
                                  case = 'parsing_error',
                                  message = error_msg,
                                  append = append,
                                  pd = NULL)
  
  solution_yaml <- tryCatch(parse_yaml(solution_code), error = function(e) {
    stop(sprintf("check_yaml() coudn't parse the solution's YAML header: %s", e$message))
  })
  
  student_yaml <- tryCatch(parse_yaml(student_code), error = function(e) {
    check_that(failure(), feedback = markdown_yaml_state$details)  
  })
  
  markdown_yaml_state$set(student_yaml = student_yaml,
                          solution_yaml = solution_yaml)
  markdown_yaml_state$set_details(case = 'correct', message = NULL)
  return(markdown_yaml_state)
}

#' Check markdown YAML header option
#'
#' Checks if a yaml header option was specified. If not, generates a feedback
#' message. If yes, zooms in on the YAML header option so you can use
#' \code{\link{check_equal}} to verify equality.
#'
#' @param state the state to start from. Should be a state produced by
#'   \code{\link{check_yaml}}.
#' @param name name of the YAML header option to zoom in on. If you want to
#'   check a nested option, use \code{c()} to chain together the different
#'   names.
#' @param not_found_msg If specified, this overrides the automatically generated
#'   message in case the option wasn't specified.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states.
#' @param ... S3 stuff
#'
#' @return A state that zooms in on the YAML header option.
#'
#' @export
check_option.MarkdownYamlState <- function(state, name, not_found_msg = NULL, append = TRUE, ...) {
  assert_state(state)
  student_yaml <- state$get("student_yaml")
  solution_yaml <- state$get("solution_yaml")
  markdown_yaml_option_state <- MarkdownYamlOptionState$new(state)
  markdown_yaml_option_state$add_details(type = 'markdown_yaml_option',
                                         case = 'defined',
                                         name = name,
                                         message = not_found_msg,
                                         append = append,
                                         pd = NULL)
  
  solution_option <- try(solution_yaml[[name]], silent = TRUE)
  if (inherits(solution_option, "try-error") || is.null(solution_option)) {
    stop(sprintf('check_option() tries to check the option %s of the yaml, but it is not specified in the solution',
                 yaml_option_desc(name)))
  }
  
  student_option <- try(student_yaml[[name]], silent = TRUE)
  check_that(is_false(inherits(student_option, "try-error") || is.null(student_option)),
             feedback = markdown_yaml_option_state$details)
  
  markdown_yaml_option_state$set(student_option = student_option,
                                 solution_option = solution_option)
  markdown_yaml_option_state$set_details(case = 'correct', message = NULL)
  return(markdown_yaml_option_state)
}

#' Check equality of markdown YAML header option
#'
#' @param state the state to start from. Should be a state produced by
#'   \code{\link{check_option}}.
#' @param incorrect_msg If specified, this overrides the automatically generated
#'   message in case the options don't match between student and solution.
#' @param append Whether or not to append the feedback to feedback built in
#'   previous states.
#' @param ... S3 stuff
#'
#' @export
check_equal.MarkdownYamlOptionState <- function(state, incorrect_msg = NULL, append = TRUE, ...) {
  assert_state(state)
  check_equal_option_helper(state, "markdown_chunk_option", incorrect_msg = incorrect_msg, append = append)
}

