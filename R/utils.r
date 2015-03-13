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

# Nicely collapse a character vector
collapse <- function(x, conn = " and ") {
  if (length(x) > 1) {
    n <- length(x)
    last <- c(n-1, n)
    collapsed <- paste(x[last], collapse = conn)
    collapsed <- paste(c(x[-last], collapsed), collapse = ", ")
  } else collapsed <- x
  collapsed
}

collapse_args <- function(x, conn = " and ") {
  collapse(paste0("<code>",x,"</code>"), conn)
}

collapse_props <- function(x, conn = " and ") {
  collapse(paste0("<code>",x,"</code>"), conn)
}

collapse_funs <- function(x, conn = " and ") {
  collapse(paste0("<code>",x,"()</code>"), conn)
}

get_num <- function(index) {
  switch(index, 
         "1" = "first", "2" = "second", 
         "3" = "third", "4" = "fourth", 
         "5" = "fifth", "6" = "sixth", 
         "7" = "seventh", sprintf("%ith", index))
}

#' convert student/solution code to vector of clean strings with the pipe operator removed.
#' @export
get_clean_lines <- function(code) {
  # convert summarize to summarise. (hacky solution)
  code = gsub("summarize","summarise",code)
  
  pd <- getParseData(parse(text = code, keep.source = TRUE))
  exprids <- pd$id[pd$parent == 0 & pd$token != "COMMENT"]
  codelines <- sapply(exprids, function(x) getParseText(pd, id = x))

  unpipedexprs <- sapply(codelines, clean_unpipe, USE.NAMES = FALSE)
  cleanlines <- gsub(" ","",unpipedexprs)

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

# Convert an expression that uses the pipe operator to a regular embedded expression.
unpipe <- function(expr) {
  cnv <- function(x) {
    lhs <- x[[2]]
    rhs <- x[[3]]

    dot_pos <- which(
      vapply(rhs
             , function(x) paste0(as.character(x), collapse = "") == "."
             , logical(1)
             , USE.NAMES = FALSE))

    if (any(all.names(rhs) == "%>%")) rhs <- decomp(rhs)
    if (any(all.names(lhs) == "%>%")) lhs <- decomp(lhs)

    # main
    if (length(dot_pos) > 0) {
      rhs[[dot_pos]] <- lhs
      rhs
    } else if (is.symbol(rhs) || rhs[[1]] == "function" || rhs[[1]] == "(") {
      as.call(c(rhs, lhs))
    } else if (is.call(rhs)) {
      as.call(c(rhs[[1]], lhs, lapply(rhs[-1], decomp)))
    } else {
      stop("missing condition error")
    }
  }

  decomp <- function(x) {
    if (length(x) == 1) x
    else if (length(x) == 3 && x[[1]] == "%>%") cnv(x)
    else if (is.pairlist(x)) as.pairlist(lapply(x, decomp))
    else as.call(lapply(x, decomp))
  }
  decomp(expr)
}