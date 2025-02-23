% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_interface.R
\name{get_subsections}
\alias{get_subsections}
\title{Get distinct subsection names for a given chapter.}
\usage{
get_subsections(chpt)
}
\arguments{
\item{chpt}{The chapter name.}
}
\value{
A data frame containing distinct subsection names in the \code{sol_subsection} column.
Returns an empty data frame if an error occurs.
}
\description{
Retrieves a list of distinct \code{sol_subsection} values for a specific
\code{sol_chapter} from the \code{mtd} table.
}
\examples{
subsections <- get_subsections("Introduction")
}
