% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treat-csv.R
\name{treat_csv}
\alias{treat_csv}
\title{Check up logbook csv file.}
\usage{
treat_csv(file, fishery = "purse_seine", species = "not_tunas", dictionaries)
}
\arguments{
\item{file}{an csv file}

\item{fishery}{fishery which the logbook treats}

\item{species}{species which the logbook treats}

\item{dictionaries}{a look-up table of vessel name and licence number in
a specific year}
}
\value{
data.frame describing error or warning records
}
\description{
Check up logbook csv file.
}
\examples{
\dontrun{
treat_csv(file = "your/file.csv",
          fishery = "purse seine",
          species = "not-tunas",
          dictionaries = list("files/at/", "your/local"))
}
}
