tw_accessors <- function() {
  tw_data <- list()
  
  get = function(name) {
    if(missing(name)) {
      tw_data
    } else {
      tw_data[[name]]
    }
  }
  
  set = function(...) {
    tw_data <<- merge(list(...))
    invisible(NULL)
  }
  
  clear = function() {
    tw_data <<- list()
    invisible(NULL)
  }
  
  initialize = function(data) {
    tw_data <<- data
    invisible(NULL)
  }
  
  merge = function(values) merge_list(tw_data, values)
  list(get = get, set = set, clear = clear, initialize = initialize)
}

merge_list <- function(x, y) {
  x[names(y)] = y
  x
}

#' "singleton" object to access all information needed by \code{\link{test_what}} functions.
#' 
#' @export
tw <- tw_accessors()

#' (re)initialize the tags for logging
#'
#' All previous tags that have been set previously will be cleared.
#' 
#' @param ... The tags you want to set, e.g. \code{fun = "test_object"}
#' @export
init_tags <- function(...) {
  tw$set(tags = list(...))
}

#' Set the tags for logging
#'
#' @param ... The tags you want to set, e.g. \code{fun = "test_object"}
#' @export
set_tags <- function(...) {
  tw$set(tags = merge_list(tw$get("tags"), list(...)))
}

# Find expression that created a variable
find_expr <- function(name, env = parent.frame()) {
  subs <- do.call("substitute", list(as.name(name), env))
  paste0(deparse(subs, width.cutoff = 500), collapse = "\n")
}

# A version of grepl that's vectorised along pattern, not x
grepl2 <- function(pattern, x, ...) {
  stopifnot(length(x) == 1)
  vapply(pattern, grepl, x, ..., FUN.VALUE = logical(1), USE.NAMES = FALSE)
}

build_pd <- function(code) {
  if(is.null(code)) {
    stop("code can't be NULL if you want to parse it")
  }
  getParseData(parse(text = code, keep.source = TRUE), includeText = TRUE)  
}


#' build R markdown document structure, using knitr functions
#' 
#' @importFrom knitr pat_md knit_patterns
#' @param text text representing an R Markdown document
build_doc_structure <- function(text) {

  # Fix markdown format
  old.format <- knitr:::opts_knit$get()
  knitr:::opts_knit$set(out.format = "markdown")
  
  # Fix pattern business
  apat = knitr::all_patterns; opat = knit_patterns$get()
  on.exit({
    knit_patterns$restore(opat)
    knitr:::chunk_counter(reset = TRUE)
    knitr:::knit_code$restore(list())
    knitr:::opts_knit$set(old.format)
  })
  pat_md()
  
  # split the file
  content = knitr:::split_file(lines = knitr:::split_lines(text)) 
  code_chunks <- knitr:::knit_code$get()
  
  for(i in seq_along(content)) {
    if(class(content[[i]]) == "block") {
      label <- content[[i]]$params$label
      content[[i]]$input <- paste(code_chunks[[label]],collapse = "\n")
    }
  }  
  
  # remove the inline blocks that contain nothing or only spaces:
  content[sapply(content, function(part) {
    all(grepl(pattern = "^\\s*$", x = part$input.src)) && class(part) == "inline"
  })] <- NULL
  
  return(content)
}

#' Parse both the student and solution document
#' 
#' @inheritParams test_function
parse_docs <- function() {
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  student_ds <- tw$get("student_ds")
  solution_ds <- tw$get("solution_ds")
  
  if(!is.null(student_ds) && !is.null(solution_ds)) {
    # Both variables exist already
    return(TRUE)
  }
  
  student_ds = build_doc_structure(student_code) #list(list(input = ""))
  solution_ds = build_doc_structure(solution_code) #list(list(input = ""))
  
  n_student = length(student_ds)
  n_solution = length(solution_ds)
  n_inline_student = sum(sapply(student_ds, class) == "inline")
  n_inline_solution = sum(sapply(solution_ds, class) == "inline")
  n_block_student = sum(sapply(student_ds, class) == "block")
  n_block_solution = sum(sapply(solution_ds, class) == "block")
  
  test_what(expect_equal(n_student, n_solution),
            sprintf("Make sure the structure of your document is OK. The solution expects %i inline (text) blocks and %i code chunks.", n_inline_solution, n_block_solution))
  
  test_what(expect_equal(n_inline_student, n_inline_solution), 
            sprintf("Make sure you have the correct amount of inline (text) blocks in your R markdown document. The solution expects %i.",n_inline_solution))
  
  test_what(expect_equal(n_block_student, n_block_solution),
            sprintf("Make sure you have the correct amount of code blocks in your R markdown document. The solution expects %i.", n_block_solution))
  
  test_what(expect_true(all.equal(sapply(student_ds, class), sapply(solution_ds, class))),
            sprintf("Make sure the overall code structure of your document is OK. The soltion expects the following setup: %s.", 
                    collapse_props(sapply(solution_ds, class), conn = ", ")))
  
  if(n_student != n_solution) return(FALSE)
  if(n_inline_student != n_inline_solution) return(FALSE) 
  if(n_block_student != n_block_solution) return(FALSE)
  if(!isTRUE(all.equal(sapply(student_ds, class), sapply(solution_ds, class)))) return(FALSE)
  
  tw$set(student_ds = student_ds)
  tw$set(solution_ds = solution_ds)
  return(TRUE)
}