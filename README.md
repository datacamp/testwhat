# testwhat

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)
[![codecov.io](https://codecov.io/github/datacamp/testwhat/coverage.svg?branch=master)](https://codecov.io/github/datacamp/testwhat?branch=master)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat?ref=badge_shield)

Verify R code submissions and auto-generate meaningful feedback messages.
Originally developed for R exercises on DataCamp for so-called Submission Correctness Tests, but can also be used independently.

- If you are new to teaching on DataCamp, check out https://instructor-support.datacamp.com.
- If you want to learn what SCTs are and how they work, visit [this article](https://instructor-support.datacamp.com/courses/course-development/submission-correctness-tests) specifically.
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

To include an SCT in a DataCamp course, visit https://instructor-support.datacamp.com.

## Tests

`testwhat` currently depends on the proprietary `RBackend` and `RCompletion` packages to run tests. Tests run automatically on every branch that is updated through travis.

```R
devtools::test()
```

## Documentation

Whenever a push is done to the `master` branch, this repo will automatically build a `pkgdown` website (containing reference documentation and vignettes), push it to the `gh-pages` branch, which in turn is served by GitHub at https://datacamp.github.io/testwhat.



## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat?ref=badge_large)