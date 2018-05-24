context("rcpp")

pec <- "library(Rcpp)"
r_part <- "# call answer and check you get the right result
x <- answer()
x"

code <- paste0("#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
int answer(){
return 42 ;
}

/*** R
", r_part, "\n*/")

test_that("parse_rcpp parses CPP code with R in it", {
  # Cast actual and expected to data.frame to avoid comparing scrfile
  #  attribute, which has a different timestamp
  expected_pd <- build_pd(r_part)
  
  s <- setup_state(
    sol_code = code,
    stu_code = code,
    pec = pec,
    ex_type = "RCppExercise"
  )
  
  actual_state <- parse_rcpp(s)
  stu_sub_pd <- data.frame(actual_state$get("student_pd"))
  sol_sub_pd <- data.frame(actual_state$get("student_pd"))
  
  expect_equal(stu_sub_pd, expected_pd, check.attributes = FALSE)
  expect_equal(sol_sub_pd, expected_pd, check.attributes = FALSE)
})