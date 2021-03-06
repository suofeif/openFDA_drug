% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_openfda.R
\name{get_openfda}
\alias{get_openfda}
\title{Extract openfda information from JSON tables exported by function fda_api_table 
to a list.}
\usage{
get_openfda(g_table, id = NULL)
}
\arguments{
\item{g_table}{a JSON table returned by fda_api_table}

\item{id}{an integer vector, for subsetting patient observations by specifying 
their IDs. If not specified, return drug information of all patients passed by 
g_table.}
}
\value{
a list of openfda data; id is the name of the list.
}
\description{
Extract openfda information from JSON tables exported by function 
fda_api_table to a list.
}
\examples{
g_table <- fda_api_table(limit=100, patientsex = 1)
get_openfda(g_table)
}
\keyword{FDA}
