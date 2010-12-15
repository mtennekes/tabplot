\name{num2fac}
\alias{num2fac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function that creates a factor vector out of a numerical vector.
}
\description{
Creates a factor vector out of a numerical vector. The levels can be determined by several methods. This function can be called via \code{\link{tableGUI}}.
}
\usage{
num2fac(num, method="pretty", num_scale="auto", n=0, brks=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
	numeric vector
}
  \item{method}{
		\itemize{
		\item \code{"pretty"} intervals are determined by the base function \code{\link{pretty}}
		\item \code{"kmeans"} the method intervals are determined by the method kmeans where \code{n} clusters (i.e. intervals) are found
		\item \code{"fixed"} determines the intervals by the argument \code{brks}
		\item \code{"discrete"} the unique values in \code{num} are mapped one to one to the levels of the new factor vector)
		}
}


  \item{num_scale}{
		\itemize{
		\item \code{"auto"} used scale is determined automatically
		\item \code{"lin"} \code{num} is directly fed to the method pretty or kmeans
		\item \code{"log"} a logarithmic transformation of \code{num} is fed to the method pretty or kmeans
		}
}
  \item{n}{
	the (desired) number of levels. \code{n=0} means automatic
}
  \item{brks}{
	breaks that determine the levels (only required when \code{method="fixed"})
}
}
\value{
	A factor vector
}
\note{
\code{ff} vectors are not implemented yet
}