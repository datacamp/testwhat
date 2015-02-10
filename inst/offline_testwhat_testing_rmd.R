#testing testwhat

###################################
#' Override library function
library <- function(package, ..., pos = NULL) {
  if (is.null(pos)) {
    pos <- grep("env:", search())
    pos <- if (length(pos) == 0) 2 else max(pos) + 1
  }
  base::library(package = as.character(substitute(package)), ..., character.only = TRUE, pos=pos)
}

library("datacampSCT")
library("testwhat")
library("knitr")

rm(list = ls())
rm(list = ls(envir = get_solution_env(), all.names = TRUE),
   envir = get_solution_env(), inherits = FALSE)

get_output <- function(code) {
  output = try(capture.output(try(eval(parse(text=code)))))
  if (inherits(output, "try-error")) {
    return("code contains an error")
  }
  return(paste(output, collapse=''))
}
####################################
assign('pre_exercise_code', '
  # pre exercise code comes here
',env=globalenv())


#SOLUTION CODE
set_solution_code('

Tryout code

```{r}
x <- 5
```

')

# USER CODE
set_student_code('
                 
Tryout code

```{r}
x <- mean(1:3)
```

')

sct = '

test_error()
test_rmd_group(2, {
test_object("x")
test_function("mean","x")
})

success_msg("Great job! Continue to the next exercise!")

'

###################################
eval(parse(text = pre_exercise_code), envir = globalenv())
res = try(knit(text = get_student_code(), envir = globalenv(), quiet = TRUE))
if(inherits(res, "try-error")) {
  set_student_error(TRUE)
} else {
  set_student_error("contains an error")
}
assign("DM.user.code", get_student_code(), envir = globalenv())
eval(parse(text = pre_exercise_code),envir = get_solution_env())
x = knit(text = get_solution_code(), envir = get_solution_env(), quiet = TRUE)

print(testwhat:::test_exercise(sct))
###################################
