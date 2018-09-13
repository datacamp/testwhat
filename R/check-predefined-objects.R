#' Test predefined R objects
#'
#' At the start of your SCT, you typically want to check whether some predefined
#' variables are still correct. \code{test_predefined_object} allows you to
#' specify a vector of object names, together with a vector of equivalence
#' conditions, evaluation specifications, undefined an incorrect messages.
#'
#' @param state the state to start from
#' @param name vector of names of the objects to check
#' @param eq_condition character vector indicating how to compare. See
#'   \code{\link{is_equal}}.
#' @param eval logical vector indicating whether or not you want to check only
#'   the objects' existence or also whether their values match the solution.
#' @param undefined_msg vector version of \code{undefined_msg} of
#'   \code{\link{check_object}}
#' @param incorrect_msg vector version of \code{incorrect_msg} of
#'   \code{\link{check_object}}
#'
#' @examples
#' \dontrun{
#' # Suppose the sample code specifies the variables a, b and c,
#' # and you want to check that a, b and c haven't changed.
#' ex() %>% check_predefined_objects(c("a", "b", "c"))
#' }
#'
#' @export
check_predefined_objects <- function(state,
                                     name, 
                                     eq_condition = "equivalent",
                                     eval = TRUE,
                                     undefined_msg = NULL, 
                                     incorrect_msg = NULL) {
  assert_state(state)
  n_names <- length(name)
  eq_condition <- rep(eq_condition, n_names, length.out = n_names)
  eval <- rep(eval, n_names, length.out = n_names)
  
  already <- "it has already been coded for you! You can use the arrow next to 'Submit Answer' to reset your code."
  
  if (is.null(undefined_msg)) {
    undefined_msg <- sprintf("Don't remove the predefined variable `%s`; %s", name, already)
  }
  
  if (is.null(incorrect_msg)) {
    incorrect_msg <- sprintf("Don't change the contents of the predefined variable `%s`; %s", name, already)
  }
  
  undefined_msg <- rep(undefined_msg, n_names, length.out = n_names)
  incorrect_msg <- rep(incorrect_msg, n_names, length.out = n_names)
  
  for (i in 1:n_names) {
    obj <- ex() %>% check_object(name[i], undefined_msg = undefined_msg[i])
    if (isTRUE(eval[i])) {
      obj %>% check_equal(eq_condition = eq_condition[i], incorrect_msg = incorrect_msg[i])
    }
  }
}

test_predefined_objects <- function(name, 
                                    eq_condition = "equivalent",
                                    eval = TRUE,
                                    undefined_msg = NULL, 
                                    incorrect_msg = NULL) {
  fail_if_v2_only()
  ex() %>% check_predefined_objects(name = name,
                                    eq_condition = eq_condition,
                                    eval = eval,
                                    undefined_msg = undefined_msg,
                                    incorrect_msg = incorrect_msg)
}