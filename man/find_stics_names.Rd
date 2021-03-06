% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_stics_names.R
\name{find_stics_names}
\alias{find_stics_names}
\title{Shiny gadget for finding Stics names}
\usage{
find_stics_names(viewer = "pane")
}
\arguments{
\item{viewer}{viewer name to choose between "pane" (default) or "dialog"}
}
\description{
Mini user interface for searching Stics
parameters or variable names and formatting them for
inserting in Rmarkdown documents.
}
\details{
Possible selection of names type (parameter or variable),
Stics versions, formatting type.
And the names search is based on partial matching using case sensitive or not,
name starting matching or not.
}
\examples{
\dontrun{

find_stics_names()

find_stics_names( viewer = "dialog" )

}
}
