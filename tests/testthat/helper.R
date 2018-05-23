library(testwhat)

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

test_it <- function(lst) {
  ex_type <- lst$DC_TYPE %||% "NormalExercise"
  setup_state(sol_code = lst$DC_SOLUTION %||% "",
              stu_code = lst$DC_CODE %||% "",
              pec <- lst$DC_PEC %||% "",
              ex_type = ex_type)

  testwhat:::post_process(run_until_fail(parse(text = lst$DC_SCT %||% "")),
                          ex_type = ex_type)
}

passes <- function(res, mess_patt = NULL) {
  expect_true(res$correct)
  if (!is.null(mess_patt)) {
    expect_true(grepl(mess_patt, res$message))
  }
}

passes2 <- function(res) {
  expect_true(inherits(res, "State"))
}

fails <- function(res, mess_patt = NULL) {
  expect_false(res$correct)
  if (!is.null(mess_patt)) {
    expect_true(grepl(mess_patt, res$message))
  }
}

fb_contains <- function(res, mess_patt, fixed = TRUE) {
  expect_true(grepl(mess_patt, res$message, fixed = fixed))
}

fb_excludes <- function(res, mess_patt, fixed = TRUE) {
  expect_false(grepl(mess_patt, res$message, fixed = fixed))
}

line_info <- function(res, line_start, line_end, column_start, column_end) {
  expect_equal(res$line_start, line_start)
  expect_equal(res$line_end, line_end)
  if(!missing(column_start)) expect_equal(res$column_start, column_start)
  if(!missing(column_end)) expect_equal(res$column_end, column_end)
}

print_fb <- function(output) {
  cat("\n", "FBM: \"", testwhat:::trim(res$message), "\"\n", sep = "")
}