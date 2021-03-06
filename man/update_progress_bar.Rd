% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{update_progress_bar}
\alias{update_progress_bar}
\title{Updates a console progress bar}
\usage{
update_progress_bar(pos, len)
}
\arguments{
\item{pos}{a numeric value greater than zero, such that pos<=len. 
Corresponds to the progress made through the function.}

\item{len}{a numberic value greater than zero, such that pos<=len.}
}
\description{
A helper function used by `number_passes`. Used
to create and update a console progress bar. For each for each
additional percent through len pos gets, a '.' is printed to
the console. Every ten percent the current progress is printed
to the screen.
}
\seealso{
number_passes
}
