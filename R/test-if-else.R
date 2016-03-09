#' Test a conditional statement
#' 
#' Check whether the student correctly coded a conditional statement. 
#' The function parses all \code{if-else} constructs and then runs tests for 
#' all composing parts of this constructions.
#' 
#' If there's an \code{else if} in there, this counts as a 'sub-conditional'
#' statement (see example).
#' 
#' @param index The index of the control structure to check.
#' @param if_cond_test tests to perform in the if condition part 
#' of the control structure
#' @param if_expr_test tests to perform in the if expression part 
#' of the control structure
#' @param else_expr_test tests to perform in the else expression 
#' part of the control structure
#' @param not_found_msg Message in case the control structure 
#' (at given index) is not found.
#' @param missing_else_msg Messing in case the else part of the 
#' control structure should be there but is missing
#' @param env  Environment in which to perform all these SCTs
#' 
#' @examples
#' \dontrun{
#' # Example solution code
#' vec <- c("a", "b", "c")
#' if("a" %in% vec) {
#'  print("a in here")
#' } else if(any("b" > vec)) {
#'  cat("b not smallest")
#' } else {
#'  str(vec)
#' }
#' 
#' # SCT to test this loop
#' test_if_else({
#'  test_student_typed("%in%")
#' }, {
#'  # test if expr part
#'  test_function("print")
#' }, {
#'  # test else expr part
#'  test_if_else({
#'    # test cond part of else if
#'    test_student_typed(">")
#'  }, {
#'    # test else if expr part
#'    test_function("cat")
#'  }, {
#'    # test else part
#'    test_function("str")
#'  })
#' })
#' }
#' 
#' @export
test_if_else <- function(index = 1, 
                         if_cond_test = NULL, 
                         if_expr_test = NULL, 
                         else_expr_test = NULL,
                         not_found_msg = NULL,
                         missing_else_msg = NULL,
                         env = parent.frame()) {
  
  student_code <- tw$get("student_code")
  solution_code <- tw$get("solution_code")
  init_tags(fun = "test_if_else")
  
  if_cond_test <- substitute(if_cond_test)
  if (is.character(if_cond_test)) code <- parse(text = if_cond_test)
  
  if_expr_test <- substitute(if_expr_test)
  if (is.character(if_expr_test)) if_expr_test <- parse(text = if_expr_test)
  
  else_expr_test <- substitute(else_expr_test)
  if (is.character(else_expr_test)) else_expr_test <- parse(text = else_expr_test)
  
  stud_pd <- getParseData(parse(text = paste(get_clean_lines(student_code), collapse = "\n"), keep.source = TRUE))
  student_structs <- extract_control_wrapper(0, stud_pd)
  sol_pd <- getParseData(parse(text = paste(get_clean_lines(solution_code), collapse = "\n"), keep.source = TRUE))
  solution_structs <- extract_control_wrapper(0, sol_pd)
  
  if(is.null(not_found_msg)) {
    not_found_msg <- sprintf(paste("The system wants to test if the %s control construct",
                                  "you coded is correct, but it hasn't found it. Add more code."), 
                            get_num(index))
  }
  
  
  ok = test_sufficient_length(student_structs, index, 
                              incorrect_number_of_calls_msg = not_found_msg)
  if(isTRUE(ok)) {
    stud_str <- student_structs[[index]]
    sol_str <- solution_structs[[index]]
    additionaltext <- sprintf(" in the %s control construct of your submission", get_num(index))
  } else {
    return(FALSE)
  }

  on.exit({
    tw$set(student_code = student_code)
    tw$set(solution_code = solution_code)
  })
  
  # IF condition part should always be there
  if(!is.null(if_cond_test) && !is.null(stud_str$if_cond) && !is.null(sol_str$if_cond)) {
    tw$set(student_code = stud_str$if_cond)
    tw$set(solution_code = sol_str$if_cond)
    eval(if_cond_test, envir = env)
  }
      
  # IF expression part should always be available.
  if(!is.null(if_expr_test) && !is.null(stud_str$if_expr) && !is.null(sol_str$if_expr)) {
    tw$set(student_code = stud_str$if_expr)
    tw$set(solution_code = sol_str$if_expr)
    eval(if_expr_test, envir = env)
  }
      
  
  if(!is.null(else_expr_test)) {
    if(is.null(missing_else_msg)) {
      missing_else_msg = sprintf("The <code>else</code> part%s is missing.", additionaltext)
    }
    test_what(expect_false(is.null(stud_str$else_expr)), missing_else_msg)
    if(!is.null(stud_str$else_expr)) {
      tw$set(student_code = stud_str$else_expr)
      tw$set(solution_code = sol_str$else_expr)
      eval(else_expr_test, envir = env)
    }
  }
}

extract_control_wrapper <- function(parent_id, pd) {
  if(any(pd$token == "IF")) {
    top_ids <- pd$id[pd$parent == parent_id]
    structs <- lapply(top_ids, extract_control, pd)
    structs <- structs[!sapply(structs, is.null)]  
    if(length(structs) == 0) {
      return(unlist(lapply(top_ids, extract_control_wrapper, pd), recursive = FALSE))
    } else {
      return(structs)
    }
  } else {
    return(list())
  }
}

extract_control <- function(parent_id, pd) {
  if(length(pd$id[pd$token == "IF" & pd$parent == parent_id]) == 0) {
    return(NULL)
  }
  
  if_exprs_ids <- pd$id[pd$token == "expr" & pd$parent == parent_id]
  
  if_cond <- getParseText(pd, if_exprs_ids[1])
  if_expr <- getParseText(pd, if_exprs_ids[2])
  
  if(length(pd$id[pd$token == "ELSE" & pd$parent == parent_id]) == 0) {
    return(list(if_cond = if_cond, if_expr = if_expr, else_expr = NULL))
  }
  
  else_expr <- getParseText(pd, if_exprs_ids[3])
  
  return(list(if_cond = if_cond, if_expr = if_expr, else_expr = else_expr))
}


# extract_control <- function(parent_id, pd) {
#   if(length(pd$id[pd$token == "IF" & pd$parent == parent_id]) == 0) {
#     return(NULL)
#   }
#   
#   if_exprs_ids <- pd$id[pd$token == "expr" & pd$parent == parent_id]  
#   if_cond <- getParseText(pd, if_exprs_ids[1])
#   
#   if(length(pd$id[pd$token == "IF" & pd$parent == if_exprs_ids[2]]) > 0) {
#     if_expr <- extract_control(parent_id = if_exprs_ids[2], pd)
#   } else {
#     if_expr <- getParseText(pd, if_exprs_ids[2])
#   }
#   
#   if(length(pd$id[pd$token == "IF" & pd$parent == parent_id]) == 0) {
#     return(list(if_cond = if_cond, if_expr = if_expr))
#   }
#   
#   if(length(pd$id[pd$token == "IF" & pd$parent == if_exprs_ids[3]]) > 0) {
#     else_expr <- extract_control(parent_id = if_exprs_ids[3], pd)
#   } else {
#     else_expr <- getParseText(pd, if_exprs_ids[3])
#   }  
#   return(list(if_cond = if_cond, if_expr = if_expr, else_expr = else_expr))
# }

# # library(datacampAPI)
# # set_student_code(paste(readLines("R/aaaaaa-examples.R"), collapse = "\n"))
# pd <- getParseData(parse(text = readlines("R/aaaaa-playing.R")))
# # 
# print_children <- function(parent_id, indent = "") {
#   children <- pd$id[pd$parent == parent_id]
#   for(c in children) {
#     cat(indent, pd$id[pd$id == c], " - ", pd$token[pd$id == c], ": ", pd$text[pd$id == c], "\n", sep = "")
#     print_children(c, indent = paste0(indent,"____ "))
#   }
# }
# 
# print_children(0, indent = "")
# # 
# # extract_control <- function(pd, parent_id = NULL) {
# #   if(is.null(parent_id)) {
# #     parent_id <- pd$parent[pd$token == "IF"][1]
# #   }
# #   if_exprs_ids <- pd$id[pd$token == "expr" & pd$parent == parent_id]  
# #   if_cond <- getParseText(pd, if_exprs_ids[1])
# #   
# #   if(length(pd$id[pd$token == "IF" & pd$parent == if_exprs_ids[2]]) > 0) {
# #     if_expr <- extract_control(pd, parent_id = if_exprs_ids[2])
# #   } else {
# #     if_expr <- getParseText(pd, if_exprs_ids[2])
# #   }
# #   
# #   if(length(pd$id[pd$token == "IF" & pd$parent == if_exprs_ids[3]]) > 0) {
# #     else_expr <- extract_control(pd, parent_id = if_exprs_ids[3])
# #   } else {
# #     else_expr <- getParseText(pd, if_exprs_ids[3])
# #   }
# #   
# #   return(list(if_cond = if_cond, if_expr = if_expr, else_expr = else_expr))
# # }
# # 
# # 
# # x <- extract_control(pd, 92)
# # # 
# # # pd
# # # if_parent_id <- pd$parent[pd$token == "IF"]
# # # if_exprs <- pd$id[pd$token == "expr" & pd$parent %in% if_parent_id]
# # # 
# # # getParseText(pd, if_exprs[1])
# # # getParseText(pd, if_exprs[2])
# # # getParseText(pd, if_exprs[3])
# # 
# # 
# # 
# # 
# # 
# # 
# # 
