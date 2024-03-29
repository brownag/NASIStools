% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remapColumns.R
\name{renormalizeColumns}
\alias{renormalizeColumns}
\title{"Unflattening" selected columns to a table with more rows}
\usage{
renormalizeColumns(x, .map, .keys)
}
\arguments{
\item{x}{a data.frame}

\item{.map}{has format \code{list(foreign_key = list(primary_key = vector_of_column_defining_unique_record))}}

\item{.keys}{has format\code{list(new_column_name = vector_of_old_column_names)}}
}
\description{
"Unflattening" selected columns to a table with more rows
}
