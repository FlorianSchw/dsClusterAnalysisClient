% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.cutree.R
\name{ds.cutree}
\alias{ds.cutree}
\title{Cuts the tree of a hclust object}
\usage{
ds.cutree(tree = NULL, k = NULL, h = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
\item{tree}{is a string character specifying the name of the hclust object}

\item{k}{specifies the number of clusters in which the tree should be cut}

\item{h}{specifies the height of a tree at which the tree should be cut}

\item{newobj}{is the name of the new object which is created with this function}

\item{datasources}{a list of \code{\link{DSConnection-class}} objects obtained after login}
}
\value{
the object specified by the \code{newobj} argument of \code{ds.cutree} or default name \code{cutree.newobj}
}
\description{
This function is similar to the native R function from stats
}
\details{
The function calls the server-side function \code{cutreeDS} that computes the
clusters for a given number of clusters k or height h, and assigns the new object to the server-side.
The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{cutree.newobj} by default.
}
\author{
Florian Schwarz for the German Institute of Human Nutrition
}
