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

# trunc_str <- function(x,start="c") {
#   max_in <- 4
#   max_out <- 2
#   tot <- max_in + max_out
#   paste0(start,"(",
#          as.character(paste(x[1:min(max_in,length(x))], collapse = ", ")),
#          ifelse(length(x) > tot, ", ...", ""), 
#          ifelse(length(x) > max_in,
#                 paste0(", ", as.character(paste(x[(length(x) - (max_out - 1 - max(0,tot - length(x)))):length(x)], collapse = ", "))),
#                 ""),
#          ")")
# }
# 
# build_summary <- function(x, ...) UseMethod("build_summary")
# 
# build_summary.default <- function(x) {
#   toString(x, width = 300)
# }
# 
# build_summary.list <- function(x) {
#   # Back up names, recursion will mess them up otherwise
#   tmp_names <- names(x)
#   # Need to manually index using seq_along, doesn't work with element-wise lapply.
#   x <- lapply(seq_along(x), function(i) { 
#     build_summary(x[[i]]) 
#   })
#   if (!is.null(tmp_names)) {
#     x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
#   }
#   trunc_str(x,"list")
# }
# 
# build_summary.data.frame <- function(x) {
#   # Back up names, recursion will mess them up otherwise
#   tmp_names <- names(x)
#   # Need to manually index using seq_along, doesn't work with element-wise lapply.
#   x <- lapply(seq_along(x), function(i) { 
#     build_summary(x[[i]]) 
#   })
#   if (!is.null(tmp_names)) {
#     x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
#   }
#   trunc_str(x,"data.frame")
# }
# 
# build_summary.character <- function(x, ..., output = FALSE) {
#   if (output) {
#     shorten <- function(str) { 
#       paste0(substr(str, 1, 100), ifelse(nchar(str) > 100, "...", "")) 
#     }
#   } else {
#     shorten <- function(str) { 
#       paste0('"',substr(str, 1, 100), ifelse(nchar(str) > 100, "...", ""),'"') 
#     }
#   }
#   if (length(x) > 1) {
#     x <- lapply(x, shorten)
#     trunc_str(x)
#   } else {
#     shorten(x)
#   }
# }
# 
# build_summary.numeric <- function(x) {
#   if (length(x) > 1) {
#     trunc_str(x)
#   } else {
#     x
#   }
# }
# 
# build_summary.factor <- function(x) {
#   paste0("factor(",build_summary.character(as.character(x)),")")
# }
# 
# test_summary <- function(x,...) {
#   build_summary(x,...)
# }
