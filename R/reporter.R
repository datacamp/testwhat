get_line_info <- function(feedback) {
  
  # take 'highest pd' in list of feedback
  pd <- NULL
  for (i in length(feedback):1) {
    if (!is.null(feedback[[i]][["pd"]])) {
      pd <- feedback[[i]][["pd"]]
      break
    }
  }
  
  if (!isTRUE(try(is.data.frame(pd), silent = TRUE))) {
    return(NULL)
  }
  
  id <- pd$id[!(pd$parent %in% pd$id)]
  if (length(id) > 1) {
    return(list(line_start = min(pd$line1),
                column_start = min(pd$col1),
                line_end = max(pd$line2),
                column_end = max(pd$col2)))
  }
  x <- as.list(pd[pd$id == id, c("line1", "col1", "line2", "col2")])
  names(x) <- c("line_start", "column_start", "line_end", "column_end")
  x
}

#' @importFrom markdown markdownToHTML
to_html <- function(x) {
  html <- markdownToHTML(text = x, fragment.only = TRUE)
  gsub("<p>(.*?)</p>", "\\1", html) #remove <p> tags, coded by front end.
}
