![banner](https://s3.amazonaws.com/assets.datacamp.com/img/github/content-engineering-repos/testwhat_banner_v2.png)

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)
[![codecov.io](https://codecov.io/github/datacamp/testwhat/coverage.svg?branch=master)](https://codecov.io/github/datacamp/testwhat?branch=master)
[![Rdoc](http://www.rdocumentation.org/badges/version/testwhat)](http://www.rdocumentation.org/packages/testwhat)

The `testwhat` package provides rich functionality to write Submission Correctness Tests for interactive R exercises on the DataCamp platform. The package is inspired by [Hadley Wickham's `testthat` package](https://github.com/hadley/testthat), the standard for unit testing in R. For a detailed guide on how to use `testwhat`, head over to the [wiki](https://github.com/datacamp/testwhat/wiki). To learn more about course creation for DataCamp, visit our [Teach Documentation](https://www.datacamp.com/teach/documentation).

## Installation

```R
library("devtools")
install_github("datacamp/testwhat")
```

## Testing this package

Because `testwhat` depends on proprietary R packages, you can only run the tests on a system that has these packages installed. If you have the private [codecov](https://codecov.io/github/datacamp/testwhat) token, you can also upload the coverage results:

```R
# Run the tests locally
devtools::test()

# Run only a part of the tests
devtools::test(filter = "test-object")

# Upload code coverage report to codecov.io
covr::codecov(token = "private_token")
```

For more details, questions and suggestions, you can contact <b>content-engineering@datacamp.com</b>.

