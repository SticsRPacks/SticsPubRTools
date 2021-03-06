% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bookdown_example.R
\name{get_bookdown_example}
\alias{get_bookdown_example}
\title{Getting a minimal bookdown R project from rstudio GitHub}
\usage{
get_bookdown_example(dir = getwd())
}
\arguments{
\item{dir}{Target directory to store the downloaded project folder}
}
\value{
The full path of the project folder
}
\description{
Getting a minimal bookdown R project from rstudio GitHub
}
\examples{
\dontrun{

# Using the default directory (i.e. current returned by getwd())
example_dir <- get_bookdown_example()

# Or specifying a directory path as function input
# Linux
example_dir <- get_bookdown_example("/path/to/dir")

# Windows
example_dir <- get_bookdown_example("drive:/path/to/dir")
}

}
