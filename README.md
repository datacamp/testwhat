# testwhat

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)
[![codecov.io](https://codecov.io/github/datacamp/testwhat/coverage.svg?branch=master)](https://codecov.io/github/datacamp/testwhat?branch=master)
[![Rdocs](http://www.rdocumentation.org/badges/version/testwhat)](http://www.rdocumentation.org/packages/testwhat)

`testwhat` enables you to write Submission Correctness Tests (SCTs) for interactive R exercises on DataCamp.

- If you are new to teaching on DataCamp, check out https://authoring.datacamp.com.
- If you want to learn what SCTs are and how they work, visit [this article](https://authoring.datacamp.com/courses/exercises/technical-details/sct.html) specifically.
- For a complete overview of all functionality inside `testwhat` and articles about what to use when, consult https://datacamp.github.io/testwhat.

For details, questions and suggestions, [contact us](mailto:content-engineering@datacamp.com).


## Installation

```R
library("remotes")
install_github("datacamp/testwhat")
```

## Demo

Experimenting locally:

```R
library(testwhat)
setup_state(sol_code = "x <- 5",
            stu_code = "x <- 4")

ex() %>% check_object("x")
# No error: x is defined in both student and solution code

ex() %>% check_object("x") %>% check_equal()
# Error: The contents of the variable `x` aren't correct.

# Debugging state
s <- ex() %>% check_object()
s                     # only prints out state class
str(s)                # full overview of state
s$get("student_code") # access student code in state
```

To include an SCT in a DataCamp course, visit https://authoring.datacamp.com.

## Tests

`testwhat` currently depends on the proprietary `RBackend` and `RCompletion` packages to run tests. Tests run automatically on every branch that is updated through travis.

```R
devtools::test()
```

## Documentation

Whenever a push is done to the `master` branch, this repo will automatically build a `pkgdown` website (containing reference documentation and vignettes), push it to the `gh-pages` branch, which in turn is served by GitHub at https://datacamp.github.io/testwhat.

