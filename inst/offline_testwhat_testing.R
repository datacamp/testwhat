#offline testing of the testwhat package

###################################
library("testwhat")
library("datacampAPI")

clean_everything()

# Override library function
library <- function(package, ..., pos = NULL) {
  if (is.null(pos)) {
    pos <- grep("env:", search())
    pos <- if (length(pos) == 0) 2 else max(pos) + 1
  }
  base::library(package = as.character(substitute(package)), ..., character.only = TRUE, pos=pos)
}

get_output <- function(code) {
  output <- capture.output(source(file = textConnection(get_student_code()), print.eval = TRUE))
  if (inherits(output, "try-error")) {
    return("code contains an error")
  }
  return(paste(output, collapse=''))
}
####################################

assign('pre_exercise_code', '

',env=globalenv())


#SOLUTION CODE
set_solution_code('

medium <- "LinkedIn"
num_views <- 14

# Control structure for medium
if(medium == "LinkedIn") {
  print("Showing LinkedIn information")
} else if(medium == "Facebook") {
  print("Showing Facebook information")
} else {
  print("Unknown medium")
}

')

# USER CODE
set_student_code('

medium <- "LinkedIn"
num_views <- 14

# Control structure for medium
if(medium == "LinkedIn") {
  print("Showing LinkedIn information")
} else if(medium == "Facebook") {
  print("Showing Facebook information")
} else {
  print("Unknown medium")
}
  
')

sct = '

test_error()
test_if_else(index = 1, 
             if_cond_test = {
               test_student_typed(c("medium == \\"LinkedIn\\"", "\\"LinkedIn\\" == medium"),
                                  not_typed_msg = paste("Don\'t change anything about the <code>if</code> part of the first control structure,",
                                                        "it has already been coded for you."))
             },
             if_expr_test = {
               test_if_else(index = 1,
                            if_cond_test = {
                              test_student_typed(c("medium == \\"Facebook\\"", "\\"Facebook\\" == medium"),
not_typed_msg = paste("Make sure to code the correct condition for the <code>if else</code>", 
                      "part of the control structure."))

}, 
if_expr_test = {
  msg <- paste("In the <code>if else</code> part of the first control construct, make sure to print", 
               "<code>\\"Showing Facebook Information\\"</code> using the <code>print()</code> function.")
  test_function("print", "x", not_called_msg = msg, incorrect_msg = msg)
},
else_expr_test = {
  msg <- "Don\'t change anything about the <code>else</code> part of the first control structure."
  test_function("print", "x", not_called_msg = msg, incorrect_msg = msg)
})
})

success_msg("Great!")
'


###################################
eval(parse(text = pre_exercise_code), envir = globalenv())
res = try(eval(parse(text = get_student_code()), envir = globalenv()))
if(inherits(res, "try-error")) {
  set_student_error("there was an error")
} else {
  set_student_error(NULL)
}
set_student_output(get_output(get_student_code()))
eval(parse(text = pre_exercise_code),envir = get_solution_env())
eval(parse(text = get_solution_code()), envir = get_solution_env())

result = test_exercise(sct)
print(result)
###################################


