no_msg <- "No message for the specified language. Make sure you specify a valid language in `set_language()`."

# Nicely collapse a character vector
collapse <- function(x, conn = " and ") {
  if (length(x) > 1) {
    n <- length(x)
    last <- c(n - 1, n)
    collapsed <- paste(x[last], collapse = conn)
    collapsed <- paste(c(x[-last], collapsed), collapse = ", ")
  } else collapsed <- x
  collapsed
}

collapse_args <- function(x, conn = " and ") {
  collapse(paste0("`",x,"`"), conn)
}

collapse_props <- function(x, conn = " and ") {
  collapse(paste0("<code>",x,"</code>"), conn)
}

collapse_funs <- function(x, conn = " and ") {
  collapse(paste0("<code>",x,"()</code>"), conn)
}

#' Specify the language for the automated feedback
#' 
#' @param lang shorthand notation for the language. Currently, "en", "fr" and "es" are supported
#' @export
set_language <- function(lang = c("en", "fr", "es")) {
  lang <- match.arg(lang)
  tw$set(language = lang)
}

reset_language <- function() {
  tw$set(language = NULL)
}

get_language <- function() {
  lang <- tw$get("language")
  ifelse(is.null(lang), "en", lang)
}

get_num <- function(index) {
  switch(index,
         "1" = "first", "2" = "second",
         "3" = "third", "4" = "fourth",
         "5" = "fifth", "6" = "sixth",
         "7" = "seventh", sprintf("%ith", index))
}

trunc_str <- function(x,start="c") {
  max_in <- 4
  max_out <- 2
  tot <- max_in + max_out
  paste0(start,"(",
         as.character(paste(x[1:min(max_in,length(x))], collapse = ", ")),
         ifelse(length(x) > tot, ", ...", ""), 
         ifelse(length(x) > max_in,
                paste0(", ", as.character(paste(x[(length(x) - (max_out - 1 - max(0,tot - length(x)))):length(x)], collapse = ", "))),
                ""),
         ")")
}


