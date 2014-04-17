\name{savePrepare}
\alias{savePrepare}
\title{Saves a prepared object}
\usage{
savePrepare(tp, dir, overwrite = FALSE)
}
\arguments{
  \item{tp}{the prepared object (created by
  \code{\link{tablePrepare}})}

  \item{dir}{directory of the prepared object}

  \item{overwrite}{logical. If \code{dir} already contains
  files of a prepared object, should they be overwritten?}
}
\description{
Saves a prepared object that has been created by
\code{\link{tablePrepare}}. If \code{\link{tablePrepare}}
is called with \code{dir} specified, then it is already
saved using this function.
}

