build_pd <- function(code) {
  if(is.null(code)) {
    stop("code can't be NULL if you want to parse it")
  }
  getParseData(parse(text = code, keep.source = TRUE), includeText = TRUE)  
}

get_children <- function(pd, id) {
  all_childs <- c()
  childs <- function(index){
    kids <- pd$id[ pd$parent %in% index ]
    if( length(kids) ){
      all_childs <<- c(all_childs, kids )
      childs( kids )
    }
  }
  childs(id)
  return(all_childs)
}

# extract_function_definition <- function() {
#   
# }