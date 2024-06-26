% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{format_cfa_partinv}
\alias{format_cfa_partinv}
\title{Extract and format parameter values for \code{PartInv}.}
\usage{
format_cfa_partinv(obj, comp)
}
\arguments{
\item{obj}{lavaan CFA output}

\item{comp}{a string indicating the lavaan object component of interest
e.g., "se", "est"}
}
\value{
The output will be a list of 5 elements:
\item{nu}{A list of length \code{g} containing \verb{1 x n} measurement intercept
vectors where \code{g} is the number of groups and \code{n} is the number of items
in the scale.}
\item{alpha}{A list of length \code{g} containing \verb{1 x d} latent factor mean
vectors where \code{g} is the number of groups and \code{d} is the number of
latent dimensions.}
\item{lambda}{A list of length \code{g} containing \verb{n x d} factor loading
matrices where \code{g} is the number of groups, \code{d} is the number of
latent dimensions, and \code{n} is the number of items in the scale.}
\item{psi}{A list of length \code{g} containing \verb{d x d} latent factor
variance-covariance matrices where \code{g} is the number of groups and \code{d}
is the number of latent dimensions.}
\item{theta}{A list of length \code{g} containing \verb{1 x n} vectors or \verb{n x n}
matrices of unique factor variances and covariances, where \code{g} is the
number of groups and \code{n} is the number of items in the scale.}
}
\description{
\code{format_cfa_partinv} takes in a lavaan CFA fit object and a component
and returns the necessary inputs for PartInv in a list
}
