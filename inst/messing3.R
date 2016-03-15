input <- "

# Create the object x
x <- mean(1:10,
          trim = 0.1)

# Update the object x
x = 52

# Update the object y
23 -> x

x <- list()

x[['b']] <- 'test'

'test2' -> x[['c']]

# Print the object x
x

mean(x <- 1:3)
"

pd <- getParseData(parse(text = input, keep.source = TRUE), includeText = TRUE)

str(get_assignments("x", pd))
