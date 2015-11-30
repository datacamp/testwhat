#' @export
test_operator <- function(op,
                          index = 1,
                          left = {},
                          right = {},
                          student_code = get_student_code(),
                          solution_code = get_solution_code(),
                          student_env = .GlobalEnv,
                          solution_env = get_solution_env()) {
  parse_stud <- parse(text = student_code)
  parse_sol <- parse(text = solution_code)
  
  parse_stud <- parse_stud[as.logical(lapply(parse_stud, function(x) x[[1]] == op))][[index]]
  parse_sol <- parse_sol[as.logical(lapply(parse_sol, function(x) x[[1]] == op))][[index]]
  
  set_student_code(paste(deparse(parse_stud[[2]]), collapse = "\n"))
  set_solution_code(paste(deparse(parse_sol[[2]]), collapse = "\n"))
  eval(left, envir = student_env)
  set_student_code(paste(deparse(parse_stud[[3]]), collapse = "\n"))
  set_solution_code(paste(deparse(parse_sol[[3]]), collapse = "\n"))
  eval(right, envir = student_env)
  set_student_code(student_code)
  set_solution_code(solution_code)
}

test_operator_rec <- function(op, parsed, parse_sol) {
  if (is.call(parsed)) {
    if (left[[1]] == op) {
      left <- parse[[2]]
    } else {
      set_student_code()
      eval(list(...)[[1]], parsed)
    }
    left <- parsed[[1]]
    if (is.call(left)) {
      if (left[[1]] == op) {
        test_operator_rec(op, ..., parsed = left)
      }
    }
  }
  args <- list(...)
}