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

# Initialize the speed variable
speed <- 64

# Code the while loop
while(speed > 30) {
  print("Slow down!")
  speed <- speed - 7
}

# Print out the speed variable
speed

')

# USER CODE
set_student_code('

# Initialize the speed variable
speed <- 64

# Code the while loop
while(speed > 30) {
  print("Slow d!")
  speed <- speed - 6
}

# Print out the speed variable
speed
  
')

sct = '

test_correct({
  test_output_contains("invisible(lapply(rep(\\"Slow down!\\", 5), print))", 
                       incorrect_msg = "Are you sure you coded the <code>while</code> loop correctly? It should print out <code>\\"Slow Down\\"</code> exactly five times!")
}, {
  test_while_loop(index = 1,
                  cond_test = {
                    test_student_typed(c("speed > 30", "30 < speed"), not_typed_msg = "Have another look at the condition of the <code>while</code> loop.")
                  },
                  expr_test = {
                    msg <- "Make sure to use the <code>print()</code> function to output \\"Slow down!\\" in each run of the <code>while</code> loop."
                    test_function("print", "x", not_called_msg = msg, incorrect_msg = msg)
                    test_student_typed(strings = "asdfasdfasdf", not_typed_msg = "Make sure to update the <code>speed</code> variable correctly inside the <code>while</code> loop")
                  })
})


test_object("speed", incorrect_msg = "After the <code>while</code> loop has executed, <code>speed</code> should equal 29. Make sure that the <code>speed</code> variable is initialized to 64 in the beginning.")
test_output_contains("speed", incorrect_msg = "Make sure to print the final contents of the <code>speed</code> variable to the console.")
success_msg("Great job! Proceed to the next exercise.")

success_msg("OKOKOK!")

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


