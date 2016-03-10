#' convert student/solution code to vector of clean strings with the pipe operator removed.
#' 
#' @param code the code to convert to a vector of unpiped clean strings 
#' @export
get_clean_lines <- function(code) {
  # convert summarize to summarise. (hacky solution)
  code = gsub("summarize","summarise",code)
  
  pd <- getParseData(parse(text = code, keep.source = TRUE))
  exprids <- pd$id[pd$parent == 0 & pd$token != "COMMENT" & pd$token != "';'"]
  codelines <- sapply(exprids, function(x) getParseText(pd, id = x))
  
  cleanlines <- sapply(codelines, clean_unpipe, USE.NAMES = FALSE)
  
  # remove "obj = " assignments: delete "=" lines and the ones preceding
  eqsubsids = which(cleanlines == "=", TRUE)
  if(length(eqsubsids) == 0) {
    return(cleanlines)
  } else {
    removeids = c(eqsubsids, eqsubsids-1)
    return(cleanlines[-removeids])
  }
}

# subfunction used by get_clean_lines to remove the pipe operator.
clean_unpipe <- function(code) {
  if(grepl("%>%",code)) {
    return(paste0(deparse(unpipe(as.call(parse(text=code))[[1]])),collapse = ''))
  } else {
    return(code)
  }
}
