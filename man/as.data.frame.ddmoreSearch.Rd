% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo.R
\name{as.data.frame.ddmoreSearch}
\alias{as.data.frame.ddmoreSearch}
\title{Represent a search result as a data.frame}
\usage{
\method{as.data.frame}{ddmoreSearch}(x, row.names = NULL,
  optional = FALSE, ...)
}
\arguments{
\item{x}{any \R object.}

\item{row.names}{\code{NULL} or a character vector giving the row
    names for the data frame.  Missing values are not allowed.}

\item{optional}{logical. If \code{TRUE}, setting row names and
    converting column names (to syntactic names: see
    \code{\link{make.names}}) is optional.  Note that all of \R's
    \pkg{base} package \code{as.data.frame()} methods use
    \code{optional} only for column names treatment, basically with the
    meaning of \code{\link{data.frame}(*, check.names = !optional)}.
    See also the \code{make.names} argument of the \code{matrix} method.}

\item{...}{additional arguments to be passed to or from methods.}
}
\description{
Represent a search result as a data.frame
}
