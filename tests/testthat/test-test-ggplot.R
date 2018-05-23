context("test_ggplot")

pec <- "library(ggplot2)"

test_that("test_ggplot works 1", {
  code <- 'ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
              geom_bar(position = "dodge") +
              scale_fill_manual("Transmission", values = c("#E41A1C", "#377EB8"), labels = c("Manual", "Automatic")) +
              scale_y_continuous("Number") +
              scale_x_discrete("Cylinders") + xlab("test")'
  res <- setup_state(code, pec = pec) %>% check_ggplot(1, check = c("geom", "scale"), exact_geom = TRUE, check_extra = "xlab")
  passes2(res)
})

test_that("test_ggplot works 2", {
  s <- setup_state(
    stu_code = 'ggplot(mtcars, aes(x = wt, y = mpg)) + geom_smooth(se = F)',
    sol_code = 'ggplot(mtcars, aes(x = wt, y = mpg)) + stat_smooth(method = "auto",se = F)',
    pec = pec
  )
  
  passes2(s %>% check_ggplot(1, check = "geom", check_geom_params = "method"))
  passes2(s %>% check_ggplot(1, check = "geom", check_geom_params = "se"))
})

test_that("spots wrong facetting (grid)", {
  code <- "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_grid(Treatment ~ Type)"
  s <- setup_state(
    stu_code = code,
    sol_code = "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_grid(. ~ Plant)",
    pec = pec
  )
  expect_error(check_ggplot(s), class = "sct_failure")
  s <- setup_state(
    stu_code = code,
    sol_code = code,
    pec = pec
  )
  passes2(check_ggplot(s))
})

test_that("spots wrong facetting (wrap)", {
  code <- "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_wrap(~ Type)"
  s <- setup_state(
    stu_code = code,
    sol_code = "ggplot(CO2, aes(conc, uptake)) + geom_point() + facet_wrap(~ Plant)",
    pec = pec
  )
  expect_error(check_ggplot(s), class = "sct_failure")
  s <- setup_state(
    stu_code = code,
    sol_code = code,
    pec = pec
  )
  passes2(check_ggplot(s))
})

test_that("can handle the pipe operator", {
  code <- "mtcars %>% filter(gear == 4) %>% ggplot(aes(x = hp, y = wt)) + geom_point()"
  s <- setup_state(
    pec = paste0(pec, "\nlibrary(dplyr)"),
    stu_code = code,
    sol_code = code
  )
  passes2(check_ggplot(s))
})

test_that("can handle british students", {
  code <- "ggplot(mtcars, aes(x = wt, y = hp)) + geom_point(aes(colour = factor(cyl)))"
  scale <-  " + scale_colour_manual(values = c('red', 'blue', 'green'))"
  s <- setup_state(
    pec = pec,
    stu_code = gsub("scale_colour", "scale_color", paste0(code, scale)),
    sol_code = paste0(code, scale)
  )
  passes2(check_ggplot(s))
  s2 <- setup_state(
    pec = pec,
    stu_code = paste0(code, scale),
    sol_code = paste0(code, scale)
  )
  passes2(check_ggplot(s2))
  s3 <- setup_state(
    pec = pec,
    stu_code = code,
    sol_code = paste0(code, scale)
  )
  expect_error(check_ggplot(s3), class = "sct_failure")
})

test_that("can handle exotic geom_labels", {
  code <- "ggplot(cars, aes(speed, dist)) + geom_label(label = rownames(cars))"
  s <- setup_state(
    pec = pec,
    stu_code = code,
    sol_code = code
  )
  passes2(check_ggplot(s))
})
