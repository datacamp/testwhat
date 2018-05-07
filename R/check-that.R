# Expectation wrapper
check_that <- function(code, feedback, env = parent.frame()) {
  
  # feedback can be a character string
  if (is.character(feedback)) {
    feedback <- list(list(message = feedback))
  }
  
  stopifnot(is.list(feedback), is.list(feedback[[1]]))

  res <- try(eval(code, envir = env), silent = TRUE)
  if (!isTRUE(res)) {
    get_rep()$register_feedback(feedback)
  }
}

# for backwords compatibility
test_what <- function(code, feedback) {
  lut <- list(expect_true = call("is_true"),
              expect_false = call("is_false"),
              expect_equal = call("is_equal"))
  call <- substitute(code)
  call[1] <- lut[[as.character(call[[1]])]]
  check_that(call, feedback, env = parent.frame())
}

is_true <- function(x) {
  identical(as.vector(x), TRUE)
}

is_false <- function(x) {
  identical(as.vector(x), FALSE)
}

is_gte <- function(x, y) {
  stopifnot(is.numeric(x), length(x) == 1)
  stopifnot(is.numeric(y), length(y) == 1)
  x >= y
}

invalid_eq_condition <- "eq_condition should be either 'equivalent', 'equal' or 'identical'."

is_equal <- function(x, y, eq_condition = "equivalent") {
  UseMethod("is_equal", x)
}

is_equal.default <- function(x, y, eq_condition = "equivalent") {
  eq_fun <- switch(eq_condition,
                   equivalent = function(x, y) isTRUE(try(all.equal(x, y, check.attributes = FALSE), silent = TRUE)),
                   equal = function(x, y) isTRUE(try(all.equal(x, y), silent = TRUE)),
                   identical = identical,
                   stop(invalid_eq_condition))
  eq_fun(x, y)
}

is_equal.formula <- function(x, y, eq_condition = "equivalent") {
  tryCatch({
    xlst <- convert_formula(x)
    ylst <- convert_formula(y)
    isTRUE(all.equal(xlst$target, ylst$target)) &&
      isTRUE(all.equal(xlst$explan, ylst$explan))
  }, error = function(e) {
    # fallback to default equality
    is_equal.default(x, y, eq_condition)
  })
}

convert_formula <- function(form) {
  n <- length(form)
  deparsed <- lapply(form, deparse)[2:n]
  target <- deparsed[[1]]
  explan <- deparsed[[2]]
  trm <- function(x) gsub("^\\s+|\\s+$", "", x)
  explan <- sort(trm(strsplit(explan, "\\+")[[1]]))
  return(list(target = target, explan = explan))
}

failure <- function() {
  FALSE
}
