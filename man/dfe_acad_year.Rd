% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfe_acad_year.R
\name{dfe_acad_year}
\alias{dfe_acad_year}
\title{Change the style of academic year}
\usage{
dfe_acad_year(year)
}
\arguments{
\item{year}{is the acadmic year we'll input, e.g. 201213 which will be converted into 2012/13}
}
\value{
Returns a character string
}
\description{
The \code{dfe_acad_year} function converts academic year numbers e.g. 201213 into strings with a forwward slash "2012/13"
}
\details{
The registers are sorted by publication date and name by
 alphabetical order. The top five registers are output as a character string
 including commas and an and, for inclusion in the report.
}
\examples{

library(dferap)
dfe_acad_year(201213)
"2012/13"


}
