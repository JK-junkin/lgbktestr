% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui-functions.R
\name{check_logbook}
\alias{check_logbook}
\title{Check up logbook file.}
\usage{
check_logbook(file, ...)
}
\arguments{
\item{file}{an input file}

\item{...}{arguments succeeded to each method function}
}
\value{
data.frame describing error or warning records
}
\description{
Check up logbook file.
}
\examples{
\dontrun{
check_logbook(file = "your/file.xlsx",
              sheet = "Sheet 1") # argument for excel file
}

}
