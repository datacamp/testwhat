#' Test predefined R objects
#' 
#' At the start of your SCT, you typically want to check whether some predefined
#' variables are still correct. \code{test_predefined_object} allows you to 
#' specify a vector of object names, together with a vector of equivalence 
#' conditions, evaluation specifications, undefined an incorrect messages. As 
#' such, \code{test_predefined_objects} is a vectorized wrapper around 
#' \code{\link{test_object}} with meaningful defeault feedback messages that 
#' tell the student to not adapt predefined objects and code in the sample code.
#' 
#' @param name vector of names of the objects to check
#' @param eq_condition vector version of \code{eq_condition} of 
#'   \code{\link{test_object}}.
#' @param eval vector version of \code{eval} of \code{\link{test_object}}.
#' @param undefined_msg vector version of \code{undefined_msg} of 
#'   \code{\link{test_object}}
#' @param incorrect_msg vector version of \code{incorrect_msg} of 
#'   \code{\link{test_object}}
#'   
#' @examples
#' \dontrun{
#' # Suppose the sample code specifies the variables a, b and c, 
#' # and you want to check that a, b and c haven't changed.
#' test_predefined_objects(c("a", "b", "c"))
#' }
#' 
#' @export
test_predefined_objects <- function(name, 
                                    eq_condition = "equivalent",
                                    eval = TRUE,
                                    undefined_msg = NULL, 
                                    incorrect_msg = NULL) {
  
  n_names <- length(name)
  eq_condition <- rep(eq_condition, n_names, length.out = n_names)
  eval <- rep(eval, n_names, length.out = n_names)
  
  already <- "it has already been coded for you! You can use the arrow next to 'Submit Answer' to reset your code."
  
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Don't remove the predefined variable `%s`;%s", name, already)
  }
  
  if (is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Don't change the contents of the predefined variable `%s`;", name, already)
  }
  
  undefined_msg <- rep(undefined_msg, n_names, length.out = n_names)
  incorrect_msg <- rep(incorrect_msg, n_names, length.out = n_names)
  
  mapply(test_object, name = name, 
         eq_condition = eq_condition, eval = eval, 
         undefined = undefined_msg, incorrect_msg = incorrect_msg)
}
