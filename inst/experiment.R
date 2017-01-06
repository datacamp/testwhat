# install packages
install <- FALSE
auth_token <- "put_auth_token_here"
if (install) {
  devtools::install_github("datacamp/r-completion", auth_token = auth_token)
  devtools::install_github("datacamp/r-backend", auth_token = auth_token, dependencies = FALSE)
  devtools::install_github("datacamp/testwhat")
}

# Source all the helper functions you need
source(file.path(system.file(package = "testwhat"), "tests", "testthat", "helper.R"))

lst <- list(DC_PEC = "y <- 6", # set to "" if not specified
            DC_SOLUTION = "x <- 10\na <- 12", # the entire code chunk is a single string
            DC_CODE = "x <- 10",
            DC_SCT = "test_object('x')")

get_sct_payload(test_it(lst))[c('correct', 'message')]
