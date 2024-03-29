% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catalog_functions.R
\name{keyword_search}
\alias{keyword_search}
\title{Global search of data catalog}
\usage{
keyword_search(keyword, ignore_case = TRUE, con = dbcon)
}
\arguments{
\item{keyword}{string or regular expression to search for}

\item{ignore_case}{logical (default=TRUE); set to FALSE to
respect case}

\item{con}{sqlite connection, default is dbcon}
}
\description{
Given a keyword, return any rows from the data catalog
tables where the keyword is found
in the sqlite connection
}
