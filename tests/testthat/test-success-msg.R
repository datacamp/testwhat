context("success_msg")

test_that("success_msg", {
  lst <- list(DC_SCT = "success_msg('this is correct')")
  output <- test_it(lst)
  passes(output, mess_patt = "This is correct")
  lst <- list(DC_SCT = "success_msg('this is correct', praise = TRUE)")
  output <- test_it(lst)
  passes(output, mess_patt = "You are")             # verify random praise
  passes(output, mess_patt = "This is correct")
})
  
