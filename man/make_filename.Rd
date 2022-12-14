% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farsfunctions.R
\name{make_filename}
\alias{make_filename}
\title{Print "make_filename"}
\usage{
make_filename(year)
}
\arguments{
\item{year}{year a character string specifying the accident year.}
}
\value{
this function returns a character vector of file name "accident_X.csv.bz2"
where X will be replaced by the year
}
\description{
This function is to take a year as an input Then create and print a name of
a file with format "accident_X.csv.bz2" where X is the input year.
It will cause error if the file of the input year does not exist
}
