% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{mn_sd_cov}
\alias{mn_sd_cov}
\title{Compute the mean, standard deviation, and covariance of latent and observed
variables.}
\usage{
mn_sd_cov(weights_item, weights_latent, alpha, psi, lambda, nu, theta)
}
\arguments{
\item{weights_item}{A vector of item weights.}

\item{weights_latent}{A vector of latent factor weights.}

\item{alpha}{A list of length \code{g} containing \verb{1 x d} latent factor mean
vectors where \code{g} is the number of groups and \code{d} is the number of latent
dimensions. The first element is assumed to belong to the reference group.}

\item{psi}{A list of length \code{g} containing \verb{d x d} latent factor
variance-covariance matrices where \code{g} is the number of groups and \code{d} is
the number of latent dimensions. The first element is assumed to belong
to the reference group.}

\item{lambda}{A list of length \code{g} containing \verb{n x d} factor loading matrices
where \code{g} is the number of groups, \code{d} is the number of latent dimensions,
and \code{n} is the number of items in the scale. The first element is assumed
to belong to the reference group.}

\item{nu}{A list of length \code{g} containing \verb{1 x n} measurement intercept
vectors where \code{g} is the number of groups and \code{n} is the number of items
in the scale. The first element is assumed to belong to the reference
group.}

\item{theta}{A list of length \code{g} containing \verb{1 x n} vectors or \verb{n x n}
matrices of unique factor variances and covariances, where \code{g} is the
number of groups and \code{n} is the number of items in the scale. The first
element is assumed to belong to the reference group.}
}
\value{
The output will be a list of 5 elements:
\item{mn_z}{Mean of the observed variable.}
\item{sd_z}{Standard deviation of the observed variable.}
\item{mn_xi}{Mean of the latent variable.}
\item{sd_xi}{Standard deviation of the latent variable.}
\item{cov_z_xi}{Covariance of the latent and observed variables.}
}
\description{
\code{mn_sd_cov} is a helper function that computes the mean,
standard deviation, and covariance of latent and observed variables for each
group.
}
