#' Test whether a student's collection of function calls follows that of the solution (dplyr exercises)
#'
#' For a specified command in the student's code, check whether the order of function calls
#' follows the one specified by the teacher. The teacher can set several orders that are accepted.
#' Only exact string matching is performed for the moment. No support for summarize(), only summarise()! 
#' (unless teacher codes it explicitly)
#'
#' @param index  exercise to be checked (solution and student code should have same number of calls!)
#' @param allow_extra  if TRUE, allows the student to define additional function, as long as the functions in
#' the solution are followed in the same order.
#' @param custom_orders  list of character vectors. Every vector represents an order that is acceptable.
#' Example of syntax to define orders: list(c("select","filter","arrange"),c("filter","arrange","select")).
#' This specific example would mean that a function call where a select inside filter inside arrange,
#' or a filter inside arrange or inside select is allowed.
#' @param incorrect_msg  feedback message in case the student order did not match any of the accepted orders.
#' @param incorrect_number_of_calls_msg  feedback message in case the student did
#' enter the same amount of commands as the solution did.
#'
#' @export
test_tree_order <- function(index = 1,
                            custom_orders = NULL,
                            allow_extra = TRUE,
                            incorrect_msg = NULL,
                            incorrect_number_of_calls_msg = NULL) {

  student_code = tw$get("student_code")
  init_tags(fun = "test_tree_order")
  
  # If necessary, build parse calls for both student code and solution code.
  pd_stud <- get_single_pd(index = index, 
                           pd = create_student_pd(student_code = student_code), 
                           incorrect_number_of_calls_msg = incorrect_number_of_calls_msg)
  if(is.null(pd_stud)) {
    return(FALSE)
  }

  # get list of functions, ordered from the inside out:
  # functions nested most deeply come first
  ordered_list = rev(pd_stud$text[pd_stud$token == "SYMBOL_FUNCTION_CALL"])

  if(is.null(incorrect_msg)) {
    incorrect_msg = sprintf("You did not use a correct function ordering in call %i of your solution. Maybe you forgot something?",index)
    if(length(custom_orders) > 1) {
      incorrect_msg = paste(incorrect_msg, "Several orders are possible!")
    }
  }

  if(!allow_extra) {
    logicals = sapply(custom_orders, function(ord,stud) (isTRUE(all.equal(ord,stud))), ordered_list)
    test_what(expect_true(any(logicals)), feedback_msg = incorrect_msg)
  } else {
    logicals = sapply(custom_orders, function(ord,stud) (isTRUE(all.equal(ord, stud[stud %in% ord]))), ordered_list)
    test_what(expect_true(any(logicals)), feedback_msg = incorrect_msg)
  }
}
