% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-rmd.R
\name{check_title}
\alias{check_title}
\title{Check markdown header title}
\usage{
check_title(state, not_found_msg = NULL, append = TRUE)
}
\arguments{
\item{state}{the state to start from. Should be a state produced by
\code{\link{check_header}}.}

\item{not_found_msg}{If specified, this overrides the automatically generated
message in case no title was specified.}

\item{append}{Whether or not to append the feedback to feedback built in
previous states.}
}
\value{
A state that zooms in on the title of the header.
}
\description{
Checks if a title was specified for a markdown header. If not, generates a
feedback message. If yes, zooms in on the title so you can use
\code{check_equal}.
}
