# Find all calls to a given function within a piece of code
#' @importFrom utils getParseText
find_function_calls <- function(pd, name, env) {
  
  # summarize vs summarise hack
  if (name == "summarise") pd$text <- gsub("summarize", "summarise", pd$text)
  if (name == "summarize") pd$text <- gsub("summarise", "summarize", pd$text)
  
  # Retrieve all function calls from parse information
  fun_ids <- pd$parent[pd$text == name & pd$token == "SYMBOL_FUNCTION_CALL"]
  
  output <- list()
  for (fun_id in fun_ids) {
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
    call <- standardize_call(as.call(expr)[[1]],expr_string,env)
    line_info <- as.list(pd[pd$id == expr_id, c("line1", "col1", "line2", "col2")])
    output <- c(output, list(c(call = call, line_info)))
  }
  return(output)
}

# Expand argument names of a function call (borrowed from pryr standardise_call)
standardize_call <- function(call, call_string, env) {
  stopifnot(is.call(call))
  
  f <- args(get(as.character(call[[1]]), env))
  
  e <- try(match.call(f, call), silent = TRUE)
  
  e <- find_S3_call(e, env = env)
  
  if (inherits(e, "try-error")) {
    test_what(fail(), 
              sprintf("There is something wrong in the following function call **%s**: _%s_", 
                      call_string,
                      attr(e,"condition")$message))
  } else {
    return(e)
  }
}

#' @importFrom utils getAnywhere methods
find_S3_call <- function(matched_call, env) {
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
    call_dispatched <- paste(call_method,call_class, sep = ".")
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
    # Nothing is done with this structure yet
    return(try(match.call(f, matched_call), silent = TRUE))
    #     return(structure(try(match.call(f, matched_call), silent = TRUE),
    #                      s3_class = call_dispatched,
    #                      s3_arg_name = as.character(names(matched_call)[[2]])))
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