input <- "
library(ggvis)
mtcars %>% 
  ggvis(~wt, ~hp) %>%
  layer_points()

mtcars %>%
  ggvis(~mpg) %>%
  layer_histograms()"

x <- getParseData(parse(text = input, keep.source = TRUE), includeText = TRUE)
