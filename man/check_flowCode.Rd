% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_params.R
\name{check_flowCode}
\alias{check_flowCode}
\title{Check flow_direction parameter}
\usage{
check_flowCode(flow_direction)
}
\arguments{
\item{flow_direction}{A character string or vector specifying the type of trade flow. Must be one or more of "import", "export", "re-export", "re-import", or "all".}
}
\value{
A character vector specifying the trade flow codes.
}
\description{
Check flow_direction parameter
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
check_flowCode("import") # returns "M"
check_flowCode(c("export", "re-export")) # returns "X,RX"
check_flowCode("trade") # throws an error because "trade" is not a valid flow code
check_flowCode(NULL) # throws an error because at least one flow code must be provided
\dontshow{\}) # examplesIf}
}
