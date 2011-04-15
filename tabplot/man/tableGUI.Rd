\name{tableGUI}
\alias{tableGUI}
\title{A graphical user interface for customizing a tableplot visualization.}
\usage{tableGUI(dat, colNames=names(dat), sortCol=1, decreasing=TRUE, scales="auto",
    pals=list(1, 9, 3, 10), nBins=100, from=0, to=100)
}
\description{A graphical user interface for customizing a tableplot visualization.}
\details{A GUI by which the user can specify tableplots. First, the user selects a data.frame (of ffdf object). Second, the user selects columns from this data.frame that are visualized. Thirds, the user customizes the tableplot by changing the number of row bins, determine which column should be sorted. Further, the user is able to create a factor vector out of a numerical vector (by means of the function \code{\link{num2fac}}).}
\alias{tableGUI}
\arguments{\item{dat}{see \code{\link{tableplot}}}
\item{colNames}{see \code{\link{tableplot}}}
\item{sortCol}{see \code{\link{tableplot}}}
\item{decreasing}{see \code{\link{tableplot}}}
\item{scales}{see \code{\link{tableplot}}}
\item{pals}{see \code{\link{tableplot}}}
\item{nBins}{see \code{\link{tableplot}}}
\item{from}{see \code{\link{tableplot}}}
\item{to}{see \code{\link{tableplot}}}
}
\examples{\dontrun{
tableGUI()
}}

