trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

capitalize <- function(x) {
  enders <- c(".", "?", "!")
  for (e in enders) {
    x <- strsplit(x, split = sprintf("\\%s\\s", e))[[1]]
    x <- paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = sprintf("%s ", e))
  }
  return(x)
}

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

collapse_props <- collapse_args

collapse_funs <- function(x, conn = " and ") {
  collapse(paste0("`",x,"()`"), conn)
}

set_language <- function(lang) {
  message("Different languages are no longer supported in testwhat")
}

get_ord <- function(index) {
  switch(index,
         "1" = "first", "2" = "second",
         "3" = "third", "4" = "fourth",
         "5" = "fifth", "6" = "sixth",
         "7" = "seventh", sprintf("%ith", index))
}

get_num <- function(index) {
  switch(index,
         "1" = "one", "2" = "two",
         "3" = "three", "4" = "four",
         "5" = "five", sprintf("%i", index)) 
}

get_times <- function(index) {
  switch(index,
         "1" = "", "2" = " twice",
         sprintf(" %s times", get_num(index)))
}

yaml_option_desc <- function(name) {
  paste0("`", paste0(name, collapse = ":"), "`")
}
