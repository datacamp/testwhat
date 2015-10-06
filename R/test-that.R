#' @export
test_that <- function(descr, code) {
  tree <- substitute(code)
  for (i in 2:length(tree)) {
    expr = tree[i]
    text = as.character(expr)
    if (grepl("expect", expr)) {
      et_pattern = "^.*(expect_\\w+\\(.*),\\s?failure_msg(\\s?=\\s?)?(.*)\\)$"
      tw_repl = "test_what(\\1), \\3)"
      repl_text = sub(et_pattern, tw_repl, text)
      eval(parse(text=repl_text))
    } else {
      eval(parse(text = text))
    }
  }
}