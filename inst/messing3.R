input <- "

a <- function(x, y) {
  return(x + y)
}

b <- function(x = 2, y) x

"

pd <- getParseData(parse(text = input, keep.source = TRUE), includeText = TRUE)

astuff <- testwhat:::extract_assignments(pd, "a")
bstuff <- testwhat:::extract_assignments(pd, "b")