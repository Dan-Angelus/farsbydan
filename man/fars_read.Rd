% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farsfunctions.R
\name{fars_read}
\alias{fars_read}
\title{Print "fars_read"}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string for the filename with a csv format that
contains a database that will be extracted to an object called "data"}
}
\value{
This function returns a data frame containing the data in the table
}
\description{
This function extracts a database from a csv file and turns it into a table,
ignoring all simple diagonistic messages and not showing any progress bar. It
returns error if the file does not exist.
}
