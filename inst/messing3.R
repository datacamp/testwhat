input <- "
(a = (2))

a <- c(1, 2, 3)

b <- 12

1 ->> a

a[a < 2] = 0

a <- 123
# some comments
names(a) <- 'test'
"

pd <- getParseData(parse(text = input, keep.source = TRUE), includeText = TRUE)

x <- testwhat:::extract_assignments(pd, "a")