#' build R markdown document structure, using knitr functions
#' 
#' @importFrom knitr pat_md knit_patterns opts_knit
#' @param text text representing an R Markdown document
build_doc_structure <- function(text) {
  
  # Fix markdown format
  old.format <- knitr::opts_knit$get()
  knitr::opts_knit$set(out.format = "markdown")
  
  # Fix pattern business
  apat = knitr::all_patterns; opat = knit_patterns$get()
  on.exit({
    knit_patterns$restore(opat)
    knitr:::chunk_counter(reset = TRUE)
    knitr:::knit_code$restore(list())
    knitr::opts_knit$set(old.format)
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
parse_docs <- function(state) {
  student_code <- state$get("student_code")
  solution_code <- state$get("solution_code")
  
  student_ds <- build_doc_structure(student_code) #list(list(input = ""))
  solution_ds <- build_doc_structure(solution_code) #list(list(input = ""))
  
  n_student <- length(student_ds)
  n_solution <- length(solution_ds)
  n_inline_student <- sum(sapply(student_ds, class) == "inline")
  n_inline_solution <- sum(sapply(solution_ds, class) == "inline")
  n_block_student <- sum(sapply(student_ds, class) == "block")
  n_block_solution <- sum(sapply(solution_ds, class) == "block")
  
  check_that(is_equal(n_student, n_solution),
            feedback = sprintf("Make sure the structure of your document is OK. The solution expects %i inline (text) blocks and %i code chunks.", n_inline_solution, n_block_solution))
  
  check_that(is_equal(n_inline_student, n_inline_solution), 
            feedback = sprintf("Make sure you have the correct amount of inline (text) blocks in your R markdown document. The solution expects %i.",n_inline_solution))
  
  check_that(is_equal(n_block_student, n_block_solution),
            feedback = sprintf("Make sure you have the correct amount of code blocks in your R markdown document. The solution expects %i.", n_block_solution))
  
  check_that(is_true(all.equal(sapply(student_ds, class), sapply(solution_ds, class))),
            feedback = sprintf("Make sure the overall code structure of your document is OK. The soltion expects the following setup: %s.", 
                               collapse_props(sapply(solution_ds, class), conn = ", ")))
  
  if(n_student != n_solution) return(FALSE)
  if(n_inline_student != n_inline_solution) return(FALSE) 
  if(n_block_student != n_block_solution) return(FALSE)
  if(!isTRUE(all.equal(sapply(student_ds, class), sapply(solution_ds, class)))) return(FALSE)
  
  state$set(student_ds = student_ds)
  state$set(solution_ds = solution_ds)
  return(state)
}