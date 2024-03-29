% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkColumnsDF1.R
\name{checkColumnsDF1}
\alias{checkColumnsDF1}
\title{Checks whether columns in a dataframe are the same and of typ 'numerical' or 'integer'}
\usage{
checkColumnsDF1(df.name, datasources)
}
\arguments{
\item{df.name}{is the name of the data frame}

\item{datasources}{a list of \code{\link{DSConnection-class}} objects obtained after login}
}
\value{
this function conducts checks only
}
\description{
This function checks a data frame and its columns which should be the same and of typ 'numerical' or 'integer'
}
\details{
The function looks for differences in data frames components.
}
\author{
Florian Schwarz for the German Institute of Human Nutrition
}
