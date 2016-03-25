![banner](https://s3.amazonaws.com/assets.datacamp.com/img/github/content-engineering-repos/testwhat_banner.png)

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)
[![codecov.io](https://codecov.io/github/datacamp/testwhat/coverage.svg?branch=master)](https://codecov.io/github/datacamp/testwhat?branch=master)

The `testwhat` package provides rich functionality to write Submission Correctness Tests for interactive R exercises on the DataCamp platform. The package is a wrapper around [Hadley Wickham's `testthat` package](https://github.com/hadley/testthat), the standard for unit testing in R. For a detailed guide on how to use `testwhat`, head over to the [wiki](https://github.com/datacamp/testwhat/wiki). To learn more about course creation for DataCamp, visit our [Teach Documentation](http://docs.datacamp.com/teach).

## Installation

```R
install.packages("devtools")
library("devtools")
install_github("datacamp/testwhat")
```

## Testing this package

Because `testwhat` depends on proprietary R packages, you can only run the tests on locally, if you have these packages installed. If you have the private Codecov token, you can upload the coverage results from your local system:

```R
# Run the tests locally
testthat::test_dir('tests/testthat')

# Upload code coverage report to codecov.io
covr::codecov(token = "private_token")
```

For more details, questions and suggestions, you can contact <b>content-engineering@datacamp.com</b>.