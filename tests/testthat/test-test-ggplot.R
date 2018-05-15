context("test_ggplot")

pec <- "suppressWarnings(library(ggplot2))"

test_that("test_ggplot works 1", {
  code <- 'ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
              geom_bar(position = "dodge") +
              scale_fill_manual("Transmission", values = c("#E41A1C", "#377EB8"), labels = c("Manual", "Automatic")) +
              scale_y_continuous("Number") +
              scale_x_discrete("Cylinders") + xlab("test")'
  lst <- list(DC_PEC = pec,
              DC_CODE = code,
              DC_SOLUTION = code,
              DC_SCT = 'test_ggplot(1, check = c("geom", "scale"), exact_geom = TRUE, check_extra = "xlab")')
  output <- test_it(lst)
  passes(output)
})

test_that("test_ggplot works 2", {
  lst <- list(
    DC_PEC = pec,
    DC_CODE = 'ggplot(mtcars, aes(x = wt, y = mpg)) + geom_smooth(se = F)',
    DC_SOLUTION = 'ggplot(mtcars, aes(x = wt, y = mpg)) + stat_smooth(method = "auto",se = F)',
    DC_SCT = 'test_ggplot(1, check = "geom", check_geom_params = "method")
              test_ggplot(1, check = "geom", check_geom_params = "se")')
  output <- test_it(lst)
  passes(output)
})

test_that('test_ggplot works 3', {
  lst <- list()
  lst$DC_PEC <- paste0(pec, "\nlibrary(RColorBrewer)\nlibrary(car)")
  lst$DC_CODE <- '
myColors <- c(brewer.pal(3, "Dark2"), "black")
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
    geom_point() +
    stat_smooth(method = "lm", se = F) +
    stat_smooth(method = "lm", se = F, span = 0.7, aes(group = 2, col = "All")) +
    scale_color_manual("Cylinders", values = myColors)'
  lst$DC_SOLUTION <- '
myColors <- c(brewer.pal(3, "Dark2"), "black")
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
    geom_point() +
    stat_smooth(method = "lm", se = F) +
    stat_smooth(method = "lm", se = F, span = 0.7, aes(group = 2, col = "All")) +
    scale_color_manual("Cylinders", values = myColors)'
  lst_DC_SCT <- '
test_ggplot(1, check = "geom", check_geom_params = c("method", "se"))
test_ggplot(1, check = "geom", check_geom_params = c("method", "se", "span", "group", "col"))
test_ggplot(1, check = "scale")'
  output <- test_it(lst)
  passes(output)
})

test_that("test_ggplot works 4", {
  lst <- list()
  lst$DC_PEC <- 'suppressWarnings(library(ggplot2))\nlibrary(RColorBrewer)\n
    z <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
    geom_point(alpha = 0.7, size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Weight (lb/1000)", y = "Miles/(US) gallon") +
    coord_cartesian(xlim = c(1,6), ylim = c(10,35)) +
    scale_color_manual("Cylinders", values = brewer.pal(9, "Blues")[c(4,6,8)]) +
    facet_wrap( ~ cyl, scales = "free_y")

    z + theme(panel.background = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    strip.background = element_blank(),
    plot.background = element_rect(fill = brewer.pal(3, "Reds")[1], color = "black", size = 3))

    myPink <- brewer.pal(3, "Reds")[1]
    '
  lst$DC_CODE <- 'z + theme(plot.background = element_rect(fill = myPink))'
  lst$DC_SOLUTION <- 'z + theme(plot.background = element_rect(fill = myPink))'
  lst$DC_SCT <- 'test_ggplot(1)'
  output <- test_it(lst)
  passes(output)
})

test_that("spots wrong facetting (grid)", {
  code <- "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_grid(Treatment ~ Type)"
  lst <- list(
    DC_PEC = pec,
    DC_CODE = code,
    DC_SOLUTION = "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_grid(. ~ Plant)",
    DC_SCT = "test_ggplot()"
  )
  output <- test_it(lst)
  fails(output)

  lst$DC_SOLUTION <- code
  output <- test_it(lst)
  passes(output)
})

test_that("spots wrong facetting (wrap)", {
  code <- "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_wrap(~ Type)"
  lst <- list(
    DC_PEC = pec,
    DC_CODE = code,
    DC_SOLUTION = "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_wrap(~ Plant)",
    DC_SCT = "test_ggplot()"
  )

  output <- test_it(lst)
  fails(output)

  lst$DC_SOLUTION <- code
  output <- test_it(lst)
  passes(output)
})

test_that("can handle the pipe operator", {
  code <- "mtcars %>% filter(gear == 4) %>% ggplot(aes(x = hp, y = wt)) + geom_point()"
  lst <- list(
    DC_PEC = paste0(pec, "\nlibrary(dplyr)"),
    DC_CODE = code,
    DC_SOLUTION = code,
    DC_SCT = "test_ggplot(1)"
  )
  output <- test_it(lst)
  passes(output)
})
