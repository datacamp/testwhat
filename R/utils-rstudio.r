# Find all top-level ggvis calls
find_ggvis_pds <- function(pd) {
  top_ggvis_ids <- pd[pd$parent == 0 & grepl("ggvis\\(", pd$text), "id"]
  lapply(top_ggvis_ids, get_sub_pd, pd = pd)
}

get_all_props = function(fun, call) {
  call[1] <- call("props")
  call <- call[-2]
  
  out = eval(call)
  if(inherits(out, "try-error")) {
    return(NULL)
  }
  else {
    # tidy up names and return
    names(out) = gsub(".update","",names(out))
    return(out)
  }
}

#' @importFrom lazyeval lazy_dots
props <- function (..., .props = NULL, inherit = TRUE, env = parent.frame()) {
  check_empty_args()
  args <- pluck(lazyeval::lazy_dots(...), "expr")
  all <- args_to_props(c(args, .props), env)
  structure(all, inherit = inherit, class = "ggvis_props")
}

pluck <- function (x, name) {
  lapply(x, `[[`, name)
}

check_empty_args <- function () {
  call <- sys.call(-1)
  args <- as.list(call[-1])
  is_missing <- function(x) identical(x, quote(expr = ))
  missing <- vapply(args, is_missing, logical(1))
  if (!any(missing)) 
    return(invisible(TRUE))
  stop("Extra comma at position", if (sum(missing) > 1) 
    "s", " ", paste0(which(missing), collapse = ", "), " in call to ", 
    as.character(call[[1]]), "()", call. = FALSE)
}

args_to_props <- function (args, env) {
  expr_to_prop <- function(name, expr, scale = NULL) {
    name <- strsplit(name, ".", fixed = TRUE)[[1]]
    property <- name[1]
    event <- if (length(name) > 1) 
      name[2]
    else NULL
    val <- eval(expr, env)
    prop(property, val, scale = scale, event = event, label = as.character(val))
  }
  prop_full_name <- function(p) {
    paste(c(p$property, p$event), collapse = ".")
  }
  arg_names <- names2(args)
  named_args <- args[arg_names != ""]
  unnamed_args <- args[arg_names == ""]
  named_args <- Map(named_args, names(named_args), 
                    f = function(x, name) expr_to_prop(name, x, scale = TRUE))
  unnamed_args <- lapply(unnamed_args, function(x) {
    if (uses_colon_equals(x)) {
      expr_to_prop(deparse(x[[2]]), x[[3]], scale = FALSE)
    }
    else {
      eval(x, env)
    }
  })
  is_prop <- vapply(unnamed_args, is.prop, logical(1))
  unnamed_props <- unnamed_args[is_prop]
  unnamed_values <- unnamed_args[!is_prop]
  names(named_args) <- vapply(named_args, prop_full_name, character(1))
  names(unnamed_props) <- vapply(unnamed_props, prop_full_name, 
                                 character(1))
  missing_names <- setdiff(c("x.update", "y.update"), c(names(named_args), 
                                                        names(unnamed_props)))
  if (length(unnamed_values) > length(missing_names)) {
    stop("Too many unnamed properties (can only have x and y)", 
         call. = FALSE)
  }
  names(unnamed_values) <- missing_names[seq_along(unnamed_values)]
  unnamed_values <- Map(unnamed_values, names(unnamed_values), 
                        f = function(x, name) expr_to_prop(name, x, scale = TRUE))
  c(named_args, unnamed_props, unnamed_values)
}

uses_colon_equals <- function (x) {
  is.call(x) && identical(x[[1]], quote(`:=`))
}

names2 <- function (x) {
  names(x) %||% rep("", length(x))
}

`%||%` <- function (a, b) {
  if (!is.null(a)) a else b
}
  
  