build_object_undefined_msg <- function(name) {
  template <- switch(get_language(),
                     en = "Did you define the variable `%s` without errors?",
                     fr = "Avez-vous d&#233;fini `%s` ?",
                     es = "&#191;Definiste el valor `%s`?",
                     stop(no_msg))
  sprintf(template, name)
}

build_object_incorrect_msg <- function(name) {
  template <- switch(get_language(),
                     en = "The contents of the variable `%s` aren't correct.",
                     fr = "Il semblerait que vous n'ayez pas affect&#233; la bonne valeur &#224; `%s`.",
                     es = "Parece que no asignaste el valor correcto a `%s`.",
                     stop(no_msg))
  sprintf(template, name)
}

build_function_not_called_msg <- function(name, index) {
  lang <- get_language()
  if (lang == "en") {
    msg <- sprintf("The system wants to check the %s call of `%s()`, but it hasn't found it; have another look at your code.", 
                   get_num(index), name)
  } else if (lang == "fr") {
    msg <- sprintf("Avez-vous ex&#233;cut&#233; %d fois la fonction `%s()` ?", 
                   index, name)
  } else if (lang == "es") {
    msg <- sprintf("&#191;Usaste la funci&#243;n `%s()` %s?", 
                   name, ifelse(index == 1, "una vez", sprintf("%d veces", index)))
  } else {
    stop(no_msg)
  }
  return(msg)
}

build_function_args_not_specified_msg <- function(name, args, n_args, allow_extra) {
  lang <- get_language()
  if(lang == "en") {
    msg <- sprintf("Did you specify the argument%s %s in your call of `%s()`?%s",
                   ifelse(n_args == 1, "", "s"),
                   collapse_args(args),
                   name,
                   ifelse(allow_extra, "", " You shouldn't specify any other arguments!"))
  } else if (lang == "fr") {
    msg <- sprintf("Avez-vous specifi&#233; %sargument%s %s dans la fonction `%s()` ?%s",
                   ifelse(n_args == 1, "l'", "les "),
                   ifelse(n_args == 1, "", "s"),
                   collapse_args(args, " et "),
                   name,
                   ifelse(allow_extra, "", " Ne specifiez pas d'autres arguments !"))
  } else if (lang == "es") {
    msg <- sprintf("Especifcaste %s argumento%s %s en la funci&#243;n `%s()`? %s",
                   ifelse(n_args == 1, "el", "los"),
                   ifelse(n_args == 1, "", "s"),
                   collapse_args(args, " y "),
                   name,
                   ifelse(allow_extra, "", " No especifica otros argumentos!"))
  } else {
    stop(no_msg)
  }
  return(msg)
}

build_function_incorrect_msg <- function(name, incorrect_arg) {
  lang <- get_language()
  if (lang == "en") {
    msg <- sprintf("Did you correctly specify the argument `%s` in your call of `%s()`?", 
                   incorrect_arg, name)
  } else if (lang == "fr") {
    msg <- sprintf("Avez-vous affect&#233; la bonne valeur &#224; l'argument `%s` dans la fonction `%s()` ?",
                   incorrect_arg, name)
  } else if (lang == "es") {
    msg <- sprintf("&#191;Usaste el valor correcto para el argumento `%s` en la funci&#243;n `%s()`?",
                   incorrect_arg, name)
  } else {
    stop(no_msg)
  }
  return(msg)
  
}
  
build_summary <- function(x, ...) UseMethod("build_summary")

build_summary.default <- function(x) {
  toString(x, width = 300)
}

build_summary.list <- function(x) {
  # Back up names, recursion will mess them up otherwise
  tmp_names <- names(x)
  # Need to manually index using seq_along, doesn't work with element-wise lapply.
  x <- lapply(seq_along(x), function(i) { 
    build_summary(x[[i]]) 
  })
  if (!is.null(tmp_names)) {
    x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
  }
  trunc_str(x,"list")
}

build_summary.data.frame <- function(x) {
  # Back up names, recursion will mess them up otherwise
  tmp_names <- names(x)
  # Need to manually index using seq_along, doesn't work with element-wise lapply.
  x <- lapply(seq_along(x), function(i) { 
    build_summary(x[[i]]) 
  })
  if (!is.null(tmp_names)) {
    x <- paste(lapply(seq_along(x), function(i) { ifelse(nchar(tmp_names[i]) != 0, paste0(tmp_names[i], " = ", x[i]), paste0(x[i])) }))
  }
  trunc_str(x,"data.frame")
}

build_summary.character <- function(x, ..., output = FALSE) {
  if (output) {
    shorten <- function(str) { 
      paste0(substr(str, 1, 100), ifelse(nchar(str) > 100, "...", "")) 
    }
  } else {
    shorten <- function(str) { 
      paste0('"',substr(str, 1, 100), ifelse(nchar(str) > 100, "...", ""),'"') 
    }
  }
  if (length(x) > 1) {
    x <- lapply(x, shorten)
    trunc_str(x)
  } else {
    shorten(x)
  }
}

build_summary.numeric <- function(x) {
  if (length(x) > 1) {
    trunc_str(x)
  } else {
    x
  }
}

build_summary.factor <- function(x) {
  paste0("factor(",build_summary.character(as.character(x)),")")
}

test_summary <- function(x,...) {
  build_summary(x,...)
}
