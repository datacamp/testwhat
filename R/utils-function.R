# Find all calls to a given function within a piece of code
#' @importFrom utils getParseText
find_function_calls <- function(pd, name, env) {
  
  # summarize vs summarise hack
  if (name == "summarise") pd$text <- gsub("summarize", "summarise", pd$text)
  if (name == "summarize") pd$text <- gsub("summarise", "summarize", pd$text)
  
  # Retrieve all function calls from parse information
  fun_ids <- pd$parent[pd$text == name & pd$token == "SYMBOL_FUNCTION_CALL"]
  
  lapply(fun_ids, function(fun_id) {
    expr_id <- pd$parent[pd$id == fun_id] 
    
    # if parent expression contains %>% on left hand side ...
    siblings <- pd$id[pd$parent == pd$parent[pd$id == expr_id]]
    if ("%>%" %in% pd$text[pd$id %in% siblings] && pd$id[pd$text == "%>%" & pd$id %in% siblings] < expr_id) {
      # ... go one level up, normalize call, and return string
      expr_id <- pd$parent[pd$id == expr_id]
      expr_string <- deparse(unpipe(as.call(parse(text = getParseText(pd, expr_id)))[[1]]))
    } else {
      expr_string <- getParseText(pd, expr_id)  
    }
    expr <- parse(text = expr_string)
    original_call <- as.call(expr)[[1]]
    standard_call <- standardize_call(original_call, expr_string, env)
    function_pd <- get_sub_pd(pd = pd, expr_id)
    arg_pds <- get_args(function_pd, standard_call)
    list(call = standard_call, pd = function_pd, args = arg_pds)
  })
}

# Find all operators in the parse data
#' @importFrom utils getParseText
find_operators <- function(pd, name, env) {
  parent_ids <- pd$parent[pd$text == name]
  lapply(parent_ids, function(id) {
    call <- getParseText(pd, id)
    pd <- pd[pd$id == id, ]
    list(call = parse(text = call), pd = pd)
  })
}


clean <- function(x) {
  x <- gsub("\\s", "", x)
  gsub("'", "\"", x)
}

get_args <- function(pd, standard_call) {
  n <- length(standard_call)
  if(n == 1) {
    return(list())
  }
  
  params <- standard_call[2:n]
  args <- lapply(params, function(param) {
    id <- pd$id[clean(deparse(param)) == clean(pd$text) & pd$token == "expr"]
    list(expr = param, pd = get_sub_pd(pd, id))
  })

  # Some arguments are not named because passed via ...
  # Group these arguments in a list
  m <- length(args)
  if (is.null(names(args))) {
    # All are unnamed
    args <- list(`...` = args)
  } else {
    hits <- which(names(args) == "")
    if (length(hits) > 0) {
      # Some arguments not named
      args$`...` <- args[hits]
      args[hits] <- NULL
    }
  }
  return(args)
}


# Expand argument names of a function call (borrowed from pryr standardise_call)
standardize_call <- function(call, call_string, env) {
  stopifnot(is.call(call))
  
  f <- args(get(as.character(call[[1]]), env))
  
  e <- try(match.call(f, call), silent = TRUE)
  
  e <- find_S3_call(matched_call = e, call = call, env = env)
  
  if (inherits(e, "try-error")) {
    check_that(failure(), 
              sprintf("There is something wrong in the following function call **%s**: _%s_", 
                      call_string,
                      attr(e,"condition")$message))
  } else {
    return(e)
  }
}

#' @importFrom utils getAnywhere methods
find_S3_call <- function(matched_call, call, env) {
  if (inherits(matched_call, "try-error")) {
    return(matched_call)
  }
  call_method <- as.character(matched_call[[1]])
  met <- try(suppressWarnings(methods(call_method)), silent = TRUE)
  if (inherits(met, "try-error")) {
    return(matched_call)
  } else if (length(met) == 0) {
    return(matched_call)
  } else if (length(matched_call) < 2) {
    return(matched_call)
  } else {
    call_class <- try(class(eval(matched_call[[2]], env)), silent = TRUE)
    if (inherits(call_class, "try-error")) {
      return(matched_call)
    }
    call_dispatched <- paste(call_method, call_class, sep = ".")
    find_call <- rep(FALSE, length(met))
    for (one_call in call_dispatched) {
      find_call <- met == one_call
      if (any(find_call)) {
        call_dispatched <- one_call
        break
      }
    }
    if (!any(find_call)) {
      call_dispatched <- paste(call_method, "default", sep = ".")
      cal_class <- "default"
      find_call <- met == call_dispatched
      if (!any(find_call)) {
        # At this point, we are almost certain the call is a primitive.
        # Just ignore.
        return(matched_call)
      }
    }
    find_call <- which(find_call)
    vis <- attr(met, "info")$visible[find_call]
    if (vis) {
      f <- args(get(call_dispatched, env))
    } else {
      f <- args(getAnywhere(call_dispatched)[1])
    }
    return(try(match.call(f, call), silent = TRUE))
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