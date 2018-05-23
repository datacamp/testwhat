build_feedback_message <- function(details) {
  if (is.character(details)) {
    return(capitalize(trim(details)))
  }
  
  total_msg <- ""
  for (det in details) {
    if (!is.null(det$message)) {
      msg <- det$message
    } else {
      class(det) <- det$type
      msg <- build_message(det)
    }
    
    # only do something if msg is actually a message
    if (!is.null(msg)) {
      if (isTRUE(det$append)) {
        total_msg <- paste(total_msg, msg)
      } else {
        total_msg <- msg
      }
    }
  }
  return(capitalize(trim(total_msg)))
}

build_message <- function(det) {
  UseMethod("build_message", det)
}

build_message.default <- function(det) {
  return(NULL)
}

build_message.object <- function(det) {
  switch(det$case,
         defined = sprintf("Did you define the variable `%s` without errors?", det$name),
         correct = sprintf("The contents of the variable `%s` aren't correct.", det$name),
         equal = build_diff(sol = det$solution, stud = det$student,
                            eq_condition = det$eq_condition,
                            id = "it"),
         NULL)
}

build_message.column <- function(det) {
  switch(det$case,
         defined = sprintf("Does it contain a column `%s`?", det$name),
         correct = sprintf("The column `%s` doesn't seem to be correct.", det$name),
         equal = NULL,
         NULL)
}

build_message.element <- function(det) {
  switch(det$case,
         defined = sprintf("Does it contain an element `%s`?", det$name),
         correct = sprintf("The element `%s` doesn't seem to be correct.", det$name),
         equal = NULL,
         NULL)
}


build_message.function <- function(det) {
  switch(det$case,
         called = sprintf("Have you called `%s()`%s?", det$name, get_times(det$index)),
         correct = sprintf("Check your call of `%s()`.", det$name),
         result_runs = "Running it again threw an error.",
         result_correct = "Running it again doesn't give the correct result.",
         result_equal = build_diff(sol = det$solution, stud = det$student,
                                   eq_condition = det$eq_condition,
                                   id = "the result"),
         NULL)
}

build_message.operator <- function(det) {
  switch(det$case,
         called = sprintf("Have you used the `%s` operator%s?", det$name, get_times(det$index)),
         correct = sprintf("Have you correctly used the `%s` operator?", det$name),
         result_runs = "Running the operation again threw an error.",
         result_correct = "Running the operation again doesn't give the correct result.",
         result_equal = build_diff(sol = det$solution, stud = det$student,
                                   eq_condition = det$eq_condition,
                                   id = "the result"),
         NULL)
}

build_message.argument <- function(det) {
  msg <- NULL
  if (det$case == "specified") {
    if (det$name == "...") {
      msg <- sprintf("Did you specify any arguments that are matched to `...`?", det$name)
    } else {
      msg <- sprintf("Did you specify the argument `%s`?", det$name)
    }
  }
  if (det$case == "correct") {
    if (det$name == "...") {
      msg <- "Did you correctly specify the arguments that are matched to `...`?"
    } else {
      msg <- sprintf("Did you correctly specify the argument `%s`?", det$name)
    }
  }
  if (det$case == "equal") {
    if (!det$is_dots) {
      msg <- build_diff(sol = det$solution, stud = det$student,
                        eq_condition = det$eq_condition,
                        id = "it")  
    }
  }
  return(msg)
}

build_message.if <- function(det) {
  build_message_control(det, "if")
}

build_message.for <- function(det) {
  build_message_control(det, "for")
}

build_message.while <- function(det) {
  build_message_control(det, "while")
}

build_message_control <- function(det, type) {
  switch(det$case,
         defined = sprintf("Are you sure you coded %s %s statement%s?", get_num(det$index), type, ifelse(det$index > 1, "s", "")),
         correct = sprintf("Check the %s %s statement.", get_ord(det$index), type),
         NULL)
}

build_message.condition <- function(det) {
  "Check the condition."
}

build_message.body <- function(det) {
  "Check the body."
}

build_message.ifexpression <- function(det) {
  "Check the if part."
}

build_message.elseexpression <- function(det) {
  switch(det$case,
         defined = "The else part is missing.",
         correct = "Check the else part.",
         NULL)
}

build_message.typed <- function(det) {
  if (det$type == "typed") {
    if (det$fixed) {
      msg <- sprintf("Have you typed %s%s?", collapse_args(det$regex, conn = " or "), get_times(det$times))
    } else {
      msg <- sprintf("The system wanted to find the pattern %s%s but didn't.", collapse_args(det$regex, conn = " or "), get_times(det$times))  
    }
  }
  return(msg)
}

build_message.fundef <- function(det) {
  switch(det$case,
         defined = sprintf("Did you define the function `%s()`?", det$name),
         correcttype = sprintf("Are you sure that `%s` is a function?", det$name),
         correct = sprintf("Did you correctly define the function `%s()`?", det$name),
         arguments = "Did you specify the correct number of arguments?",
         coded = sprintf("The system couldn't find the function definition of `%s()` in your code.", det$name),
         NULL)
}

build_message.expr <- function(det) {
  switch(det$case, 
         result_runs = sprintf("Running `%s` generated an error.", det$expr_str),
         result_correct = sprintf("Running `%s` didn't give the correct result.", det$expr_str),
         result_equal = build_diff(sol = det$solution, stud = det$student,
                                   eq_condition = det$eq_condition,
                                   id = "the result"),
         output_runs = sprintf("Running `%s` generated an error.", det$expr_str),
         output_correct = sprintf("Running `%s` didn't generate the correct output.", det$expr_str),
         output_equal = sprintf("Expected %s, but got %s",
                                ifelse(length(det$solution) == 0, "no output", sprintf("`%s`", det$solution)),
                                ifelse(length(det$student) == 0, "no output", sprintf("`%s`", det$student))),
         error_fails = sprintf("Running `%s` didn't generate an error, but it should.", det$expr_str),
         error_correct = sprintf("Running `%s` didn't generate the correct error.", det$expr_str),
         error_equal = sprintf("Expected the error `%s`, but instead got the error `%s`",
                               det$solution, det$student),
         NULL)
}

build_message.file <- function(det) {
  msg <- NULL
  if (det$case == "available") {
    if (det$folder == ".") {
      msg <- sprintf("The file <code>%s</code> does not appear to be in your working directory.", det$file)
    } else {
      msg <- sprintf("The file <code>%s</code> does not appear to be inside the folder `%s` in your working directory.", det$file, det$folder)
    }
  }
  return(msg)
}

build_message.output <- function(det) {
  switch(det$case, 
         regex = "The output that your code generated doesn't contain the pattern we're looking for.",
         expr = sprintf("Did your code produce the same output as <code>%s</code>?", det$expr),
         NULL)
}
