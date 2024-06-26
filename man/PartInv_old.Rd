% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprec-PartInv.R
\name{PartInv_old}
\alias{PartInv_old}
\title{Evaluate partial measurement invariance using Millsap & Kwok's (2004)
approach}
\usage{
PartInv_old(
  propsel,
  cut_z = NULL,
  kappa_r,
  kappa_f = kappa_r,
  phi_r,
  phi_f = phi_r,
  lambda_r,
  lambda_f = lambda_r,
  Theta_r,
  Theta_f = Theta_r,
  tau_r,
  tau_f = tau_r,
  pmix_ref = 0.5,
  plot_contour = TRUE,
  labels = c("Reference group", "Focal group"),
  ...
)
}
\arguments{
\item{propsel}{proportion of selection. If missing, computed using \code{cut_z}.}

\item{cut_z}{prespecified cutoff score on the observed composite.
This argument is ignored when \code{propsel} has input.}

\item{kappa_r}{latent factor mean for the reference group.}

\item{kappa_f}{(optional) latent factor mean for the focal group;
if no input, set equal to \code{kappa_r}.}

\item{phi_r}{latent factor variance for the reference group.}

\item{phi_f}{(optional) latent factor variance for the focal group;
if no input, set equal to \code{phi_r}.}

\item{lambda_r}{a vector of factor loadings for the reference group.}

\item{lambda_f}{(optional) a vector of factor loadings for the focal group;
if no input, set equal to \code{lambda_r}.}

\item{Theta_r}{a matrix of the unique factor variances and covariances for
the reference group.}

\item{Theta_f}{(optional) a matrix of the unique factor variances and
covariances for the focal group; if no input, set equal to
\code{Theta_r}.}

\item{tau_r}{a vector of measurement intercepts for the reference group.}

\item{tau_f}{(optional) a vector of measurement intercepts for the focal
group; if no input, set equal to \code{tau_r}.}

\item{pmix_ref}{Proportion of the reference group; default to 0.5 (i.e., two
populations have equal size).}

\item{plot_contour}{logical; whether the contour of the two populations
should be plotted; default to \code{TRUE}.}

\item{labels}{a character vector with two elements to label the reference
and the focal group on the graph.}

\item{...}{other arguments passed to the \code{\link[graphics]{contour}}
function.}
}
\value{
a list of four elements and a plot if \code{plot_contour == TRUE}.
The four elements are
\describe{
\item{propsel}{echo the same argument as input}
\item{cutpt_xi}{cut point on the latent scale (xi)}
\item{cutpt_z}{cut point on the observed scale (Z)}
\item{summary}{A 8 x 2 table, with columns representing the reference
and the focal groups, and the rows represent probabilities
of true positive (A), false positive (B),
true negative (C), false negative (D); proportion selected,
success ratio, sensitivity, and specificity. }
}
}
\description{
Evaluate partial measurement invariance using Millsap & Kwok's (2004)
approach
}
\examples{
PartInv(propsel = .25, kappa_r = 0.5, kappa_f = 0, phi_r = 1,
        lambda_r = c(.3, .5, .9, .7), tau_r = c(.225, .025, .010, .240),
        Theta_r = diag(.96, 4), labels = c("female", "male"))
}
