% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_comtrade_request.R
\name{build_comtrade_request}
\alias{build_comtrade_request}
\title{Build a valid request object from the checked parameters}
\usage{
build_comtrade_request(params, primary_token = NULL, verbose = F)
}
\arguments{
\item{params}{a named vector of parameters for the comtrade request}

\item{primary_token}{Your primary token. Default is to check in environment for stored token, if not passed through the `set_primary_comtrade_key` function}

\item{verbose}{whether the function sends status updates to the console}
}
\value{
a httr2 request object
}
\description{
This function takes the necessary parameters and creates a httr2 request to be performed this request can then be used in a second function, to actually return the data
}
