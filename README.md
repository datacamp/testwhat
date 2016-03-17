![banner](https://s3.amazonaws.com/assets.datacamp.com/img/github/content-engineering-repos/testwhat_banner.png)

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)

The `testwhat` package provides rich functionality to write Submission Correctness Tests for interactive R exercises on the DataCamp platform. The package is a wrapper around [Hadley Wickham's `testthat` package](https://github.com/hadley/testthat), the standard for unit testing in R.

For a detailed guide on how to use `testwhat`, head over to the [wiki](https://github.com/datacamp/testwhat/wiki).

To learn more about course creation for DataCamp, visit our [Teach Documentation](http://docs.datacamp.com/teach).

## Installation

```R
install.packages("devtools")
library("devtools")
install_github("Data-Camp/testwhat")
```

For more details, questions and suggestions, you can contact <b>content-engineering@datacamp.com</b>.

## Testing this package

To test this package, you to have several proprietary packages installed on your system. The tests are in two places at the moment: as scenarios in the `inst` folder, and in the traditional `tests` folder.

```R
# test the scenarios
source('inst/full-test.R')

# perform the tests in the tests/ folder
testthat::test_dir('tests')
```