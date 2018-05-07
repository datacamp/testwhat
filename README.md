# testwhat

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)
[![codecov.io](https://codecov.io/github/datacamp/testwhat/coverage.svg?branch=master)](https://codecov.io/github/datacamp/testwhat?branch=master)
[![Rdocs](http://www.rdocumentation.org/badges/version/testwhat)](http://www.rdocumentation.org/packages/testwhat)

The `testwhat` package provides rich functionality to write Submission Correctness Tests for interactive R exercises on the DataCamp platform. The package is inspired by [`testthat`](https://github.com/hadley/testthat), the standard for unit testing in R.

## Reference

* Teach on DataCamp: https://authoring.datacamp.com
* What is an SCT? https://authoring.datacamp.com/courses/sct.html

## Installation

```R
library("devtools")
install_github("datacamp/testwhat")
```

## Running tests

`testwhat` currently depends on the proprietary `RBackend` and `RCompletion` packages to run tests.

```R
devtools::test()
```

For more details, questions and suggestions, [contact us](mailto:content-engineering@datacamp.com).

