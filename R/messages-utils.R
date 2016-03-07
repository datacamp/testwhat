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
  collapse(paste0("`",x,"`"), conn)
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

trunc_str <- function(x,start="c") {
  max_in <- 4
  max_out <- 2
  tot <- max_in + max_out
  paste0(start,"(",
         as.character(paste(x[1:min(max_in,length(x))], collapse = ", ")),
         ifelse(length(x) > tot, ", ...", ""), 
         ifelse(length(x) > max_in, 
                paste0(", ",as.character(paste(x[(length(x)-(max_out - 1 - max(0,tot-length(x)))):length(x)], collapse = ", "))),
                ""),
         ")")
}


