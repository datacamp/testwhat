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

# Define the function my_filter()
my_filter <- function(x) {
  if(x > 0) {
    return(x)
  } else {
    return(NULL)
  }
}

# Call the function my_filter() twice
my_filter(5)
my_filter(-5)

')

# USER CODE
set_student_code('

# Define the function my_filter()
my_filter <- function(x) {
  if(x > 0) {
    return(x)
  }
}

# Call the function my_filter() twice
my_filter(5)
my_filter(-5)
')

sct = '

test_function_definition("my_filter",
                         function_tests = {
                           have_a_look <- "Have another look at the definition of the <code>my_filter()</code> function."
                           expect_that(my_filter(5), equals(5), 
                                       failure_msg = paste(have_a_look, "Calling <code>my_filter()</code> with 5 as the input, should equal 5."))
                           expect_that(grepl("NULL", paste(capture.output(my_filter(-5)), collapse = "\n")), is_true(),
                                       failure_msg = paste(have_a_look, "Calling <code>my_filter()</code> with -5 as the input, should equal <code>NULL</code>"))
                         },
                         body_test = {
                           test_if_else(index = 1,
                                        if_cond_test = {
                                          test_student_typed(c("> 0","0 <"), 
                                                             not_typed_msg = paste("Have another look at the condition of the <code>if</code> statement",
                                                                                   "inside the definition of <code>my_filter</code>."))
                                        },
                                        else_expr_test = {
                                          test_student_typed("NULL", 
                                                             not_typed_msg="Make sure to return <code>NULL</code> explicitly in the <code>else</code> part inside <code>my_filter()</code>.")
                                        },
                                        missing_else_msg = "You should include an <code>else</code> statement inside <code>my_filter()</code>.",
                                        not_found_msg = paste("You should include an <code>if</code>-<code>else</code> statement inside",
                                                              "your defnition of <code>my_filter()</code>."))
                         })


success_msg("Truly impressive functionality! Head over to the next exercise.")

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


