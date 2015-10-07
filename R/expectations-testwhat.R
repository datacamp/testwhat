expect_defined <- function(object, env = .GlobalEnv, info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- object
  }
  expect_that(object, is_defined(env = env), info = info, label = object)
}

is_defined <- function(env = .GlobalEnv) {
  function(name) {
    ok <- exists(name, envir = env, inherits = FALSE)
    expectation(ok, "is not defined", "is defined")
  }
}