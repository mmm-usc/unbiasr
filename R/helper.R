#' Compute the mean, standard deviation, and covariance of latent and observed
#' variables.
#'
#' \code{mn_sd_cov} is a helper function that computes the mean, 
#' standard deviation, and covariance of latent and observed variables for the
#' focal and reference groups.
#' @param weights_item A vector of item weights.
#' @param weights_latent A vector of latent factor weights.
#' @param alpha_r A vector of latent factor means for the reference group.
#' @param alpha_f A vector of latent factor means for the focal group.
#' @param psi_r A matrix of latent factor variance-covariances for the
#'     reference group.
#' @param psi_f A matrix of latent factor variance-covariances for
#'     the focal group.
#' @param lambda_r A matrix of factor loadings for the reference group.
#' @param lambda_f A matrix of factor loadings for the focal group.
#' @param nu_r A matrix of measurement intercepts for the reference group.
#' @param nu_f A matrix of measurement intercepts for the focal
#'     group.
#' @param Theta_r A matrix of the unique factor variances and covariances
#'     for the reference group.
#' @param Theta_f A matrix of the unique factor variances and
#'     covariances for the focal group.
#' @return The output will be a list of 10 elements:
#'     \item{mn_z_r}{Mean of the observed variable for the reference group.}
#'     \item{mn_z_f}{Mean of the observed variable for the focal group.}
#'     \item{sd_z_r}{Standard deviation of the observed variable for the 
#'     reference group.}
#'     \item{sd_z_f}{Standard deviation of the observed variable for the focal 
#'     group.}
#'     \item{mn_xi_r}{Mean of the latent variable for the reference group.}
#'     \item{mn_xi_f}{Mean of the latent variable for the focal group.}
#'     \item{sd_xi_r}{Standard deviation of the latent variable for the reference
#'     group.}
#'     \item{sd_xi_f}{Standard deviation of the latent variable for the focal 
#'     group.}
#'     \item{cov_z_xi_r}{Covariance of the latent and observed variables for the 
#'     reference group.}
#'     \item{cov_z_xi_f}{Covariance of the latent and observed variables for the 
#'     focal group.}
mn_sd_cov <- function(weights_item, weights_latent, alpha_r, alpha_f, psi_r, 
                      psi_f, lambda_r, lambda_f, nu_r, nu_f, Theta_r, Theta_f) {
  
  # compute mean, sd for the observed variable for reference, focal groups
  mn_z_r <- c(crossprod(weights_item, nu_r + lambda_r %*% alpha_r))
  mn_z_f <- c(crossprod(weights_item, nu_f + lambda_f %*% alpha_f))
  sd_z_r <- c(sqrt(crossprod(weights_item,
                             lambda_r %*% psi_r %*% t(lambda_r) + Theta_r) %*%
                     weights_item))
  sd_z_f <- c(sqrt(crossprod(weights_item,
                             lambda_f %*% psi_f %*% t(lambda_f) + Theta_f) %*%
                     weights_item))
  
  # compute mean, sd for the latent variable for reference, focal groups
  mn_xi_r <- c(crossprod(weights_latent, alpha_r))
  mn_xi_f <- c(crossprod(weights_latent, alpha_f))
  sd_xi_r <- c(sqrt(crossprod(weights_latent, psi_r) %*% weights_latent))
  sd_xi_f <- c(sqrt(crossprod(weights_latent, psi_f) %*% weights_latent))
  
  # compute covariance for the latent and observed variables
  cov_z_xi_r <- c(crossprod(weights_item, lambda_r %*% psi_r) %*% weights_latent)
  cov_z_xi_f <- c(crossprod(weights_item, lambda_f %*% psi_f) %*% weights_latent)
  return(list(mn_z_r, mn_z_f, sd_z_r, sd_z_f, 
              mn_xi_r, mn_xi_f, sd_xi_r, sd_xi_f, 
              cov_z_xi_r, cov_z_xi_f))
}

mn_sd_cov_new <- function(weights_item, weights_latent,
                          alpha, psi, lambda, nu, Theta) {
  # compute mean, sd for the observed variable for reference, focal groups
  mn_z <- c(crossprod(weights_item, nu + lambda %*% alpha))
  sd_z <- c(sqrt(crossprod(weights_item,
                           lambda %*% psi %*% t(lambda) + Theta) %*%
                   weights_item))
  
  # compute mean, sd for the latent variable for reference, focal groups
  mn_xi <- c(crossprod(weights_latent, alpha))
  sd_xi <- c(sqrt(crossprod(weights_latent, psi) %*% weights_latent))
  
  # compute covariance for the latent and observed variables
  cov_z_xi <- c(crossprod(weights_item, lambda %*% psi) %*% weights_latent)
  return(list(mn_z = mn_z, sd_z = sd_z, mn_xi = mn_xi, sd_xi = sd_xi,
              cov_z_xi = cov_z_xi))
}

compute_cai <- function(weights_item, weights_latent,
                        alpha_r, alpha_f, psi_r, psi_f,
                        lambda_r, lambda_f, nu_r, nu_f, Theta_r, Theta_f,
                        pmix_ref, propsel, cut_z = NULL,
                        is_mi = FALSE) {
  lst_r <- mn_sd_cov_new(weights_item, weights_latent, alpha_r, 
                         psi_r, lambda_r, nu_r, Theta_r)
  lst_f <- mn_sd_cov_new(weights_item, weights_latent, alpha_f, 
                         psi_f, lambda_f, nu_f, Theta_f)
  
  # if there is an input for selection proportion
  if (!is.null(propsel)) {
    # compute the cut score using qnormmix based on input selection proportion
    cut_z <- qnormmix(propsel,
                      lst_r$mn_z, lst_r$sd_z, lst_f$mn_z, lst_f$sd_z,
                      pmix_ref, lower.tail = FALSE)
  } else if (!is.null(cut_z) & is.null(propsel)) {
    # compute the selection proportion using pnormmix based on the cutoff value
    propsel <- pnormmix(cut_z,
                        lst_r$mn_z, lst_r$sd_z, lst_f$mn_z, lst_f$sd_z,
                        pmix_ref, lower.tail = FALSE)
  }
  
  # compute the threshold for the latent variable based on the selection 
  # proportion provided by the user/computed using cut_z
  cut_xi <- qnormmix(propsel,
                     lst_r$mn_xi, lst_r$sd_xi, lst_f$mn_xi, lst_f$sd_xi,
                     pmix_ref, lower.tail = FALSE)
  
  # computing summary statistics using .partit_bvnorm
  CAI_r <- .partit_bvnorm(cut_xi, cut_z,
                          lst_r$mn_xi, lst_r$sd_xi, lst_r$mn_z, lst_r$sd_z,
                          cov12 = lst_r$cov_z_xi)
  CAI_f <- .partit_bvnorm(cut_xi, cut_z,
                          lst_f$mn_xi, lst_f$sd_xi, lst_f$mn_z, lst_f$sd_z,
                          cov12 = lst_f$cov_z_xi)
  
  # Store mean, sd, cov values for the obs/latent variables; ref/focal groups
  zf_par <- list(mn_xi_r = lst_r$mn_xi, mn_xi_f = lst_f$mn_xi, 
                 sd_xi_r = lst_r$sd_xi, sd_xi_f = lst_f$sd_xi, 
                 mn_z_r = lst_r$mn_z, mn_z_f = lst_f$mn_z,
                 sd_z_r = lst_r$sd_z, sd_z_f = lst_f$sd_z,
                 cov_z_xi_r = lst_r$cov_z_xi, cov_z_xi_f = lst_f$cov_z_xi)
  dat <- data.frame("Reference" = CAI_r, "Focal" = CAI_f,
                    row.names = c("A (true positive)", "B (false positive)",
                                  "C (true negative)", "D (false negative)",
                                  "Proportion selected", "Success ratio",
                                  "Sensitivity", "Specificity"))
  
  if (!is_mi) {
    # selection indices for the focal group if its distribution matches the
    # distribution of the reference group (Efocal)
    mn_z_Ef <- c(crossprod(weights_item, nu_f + lambda_f %*% alpha_r))
    sd_z_Ef <- c(sqrt(crossprod(weights_item, lambda_f %*% psi_r %*% t(lambda_f) 
                                + Theta_f) %*% weights_item))
    cov_z_xi_Ef <- c(crossprod(weights_item, lambda_f %*% psi_r) %*%
                       weights_latent)
    CAI_Ef <- .partit_bvnorm(cut_xi, cut_z,
                             lst_r$mn_xi, lst_r$sd_xi, mn_z_Ef, sd_z_Ef,
                             cov12 = cov_z_xi_Ef)
    dat <- cbind(dat, "E_R(Focal)" = CAI_Ef)
  }
  list(propsel = propsel, cutpt_xi = cut_xi, cutpt_z = cut_z,
       summary = dat, bivar_data = zf_par)
}

#' Distribution function (pdf) of a mixture of two normal distributions. 
#' 
#' \code{pnormmix} returns the cumulative probability of q or \eqn{1 - q} on the
#'  mixture normal distribution.
#' 
#' @param q A vector of quantiles.
#' @param mean1 Mean of the first normal distribution.
#' @param sd1 Standard deviation of the first normal distribution.
#' @param mean2 Mean of the second normal distribution.
#' @param sd2 Standard deviation of the second normal distribution.
#' @param pmix1 Mixing proportion for the first distribution. Should be a 
#'   number in the range (0, 1).
#' @param lower.tail A logical scalar; if TRUE (default), probabilities are 
#' \eqn{P[X <= x]}; otherwise, \eqn{P[X > x]}. 
#' @return The output will be the cumulative probability of q or \eqn{1 - q} on 
#' the mixture normal distribution.
#' @examples
#' \dontrun{
#' pnormmix(1, 0, 3.1, 1.7, 3.1, lower.tail = FALSE)
#' }

pnormmix <- function(q, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, pmix1 = 0.5, 
                     lower.tail = TRUE) {
  stopifnot(pmix1 > 0, pmix1 < 1)
  as.vector(c(pmix1, 1 - pmix1) %*% 
              sapply(q, pnorm, mean = c(mean1, mean2), sd = c(sd1, sd2), 
                     lower.tail = lower.tail))
}

# pnormmix(, mean1 = 1.53, sd1 = 0.89, mean2 = 1.32, sd2 = 0.88, pmix1 = 0.5, lower.tail = FALSE)

#' Quantile function of a mixture of two normal distributions. 
#' 
#' \code{qnormmix} returns the quantile corresponding to \eqn{p} or \eqn{1 - q} on 
#' the mixture normal distribution.
#' 
#' @param p A vector of probabilities.
#' @param mean1 Mean of the first normal distribution.
#' @param sd1 Standard deviation of the first normal distribution.
#' @param mean2 Mean of the second normal distribution.
#' @param sd2 Standard deviation of the second normal distribution.
#' @param pmix1 Mixing proportion for the first distribution. Should be a 
#'   number in the range (0, 1).
#' @param lower.tail A logical scalar; if TRUE (default), probabilities are 
#' \eqn{P[X <= x]}; otherwise, \eqn{P[X > x]}. 
#' @return The output will be the quantile corresponding to p or \eqn{1 - q} on 
#' the mixture normal distribution.
#' @examples
#' \dontrun{
#' qnormmix(0.8, 0, 3.1, 1.7, 0.5, lower.tail = FALSE)
#' }

qnormmix <- function(p, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, pmix1 = 0.5, 
                     lower.tail = TRUE) {
  
  stopifnot(pmix1 > 0, pmix1 < 1, p >= 0, p <= 1)
  f <- function(x) (pnormmix(x, mean1, sd1, mean2, sd2, pmix1, 
                             lower.tail) - p)^2
  start <- as.vector(c(pmix1, 1 - pmix1) %*% 
                       sapply(p, qnorm, c(mean1, mean2), c(sd1, sd2), 
                              lower.tail = lower.tail))
  nlminb(start, f)$par
}

#' Helper function for computing the kernel for bivariate normal density. 
#' 
#' \code{.bvnorm_kernel} returns the kernel for bivariate normal density
#' 
#' @param x A normal distribution
#' @param mu_x Mean of the normal distribution x.
#' @param y A normal distribution
#' @param sd_x Standard deviation of the normal distribution x.
#' @param mu_y Mean of the normal distribution y.
#' @param sd_y Standard deviation of the normal distribution y.
#' @param cov_xy covariance between x and y
#' 
#' @return The output will be the quantile corresponding to p or \eqn{1 - q} on 
#' the mixture normal distribution.
#' @examples
#' \dontrun{
#' .bvnorm_kernel(x = -2.50, y = -2.52, mu_x = 1, mu_y = 0.57, sd_x = 1,
#' sd_y = 1.03, cov_xy = 0.8)
#' }

.bvnorm_kernel <- function(x, y, mu_x = 0, mu_y = 0, sd_x = 1, sd_y = 1, 
                           cov_xy = 0) {
  
  cor <- cov_xy / sd_x / sd_y
  numer <- (x - mu_x)^2 / sd_x^2 + (y - mu_y)^2 / sd_y^2 - 
    2 * cor * (x - mu_x) * (y - mu_y) / sd_x / sd_y
  numer / (1 - cor^2)
}

#' Computing summary statistics from a selection approach
#' 
#' \code{.partit_bvnorm} returns a table of selection accuracy indices
#' 
#' @param cut1 Cut score based on the latent score
#' @param cut2 Cut score based on the observed score
#' @param mean1 Mean of first normal distribution (on x-axis).
#' @param sd1 Standard deviation of first normal distribution.
#' @param mean2 Mean of second normal distribution (on y-axis).
#' @param sd2 Standard deviation of second normal distribution.
#' @param cor12 Correlation in the bivariate normal.
#' @param cov12 Covariance in the bivariate normal. If not input, compute the
#'          covariance using the correlation and the standard deviations.
#' 
#' @return A table of selection accuracy indices
#' @examples 
#' \dontrun{
#' .partit_bvnorm(cut1 = 2, cut2 = 2, mean1 = 0, sd1 = 1, 
#'                mean2 = 1.53, sd2 = 0.89, cov12 = 0.80)
#' }

.partit_bvnorm <- function(cut1, cut2, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, 
                           cor12 = 0, cov12 = cor12 * sd1 * sd2) {
  Sigma <- matrix(c(sd1^2, cov12, cov12, sd2^2), nrow = 2)
  C <- mnormt::pmnorm(c(cut1, cut2), c(mean1, mean2), Sigma)
  B <- pnorm(cut1, mean1, sd1) - C
  D <- pnorm(cut2, mean2, sd2) - C
  A <- 1 - B - C - D
  propsel <- A + B
  success_ratio <- A / propsel
  sensitivity <- A / (A + D)
  specificity <- C / (C + B)
  c(A, B, C, D, propsel, success_ratio, sensitivity, specificity)
}

is_symmetric_posdef <- function(x, tol = 1e-08) {
  # Borrow from matrixcalc::is.positive.definite()
  if (!isSymmetric(x)) return(FALSE)
  eigenvalues <- eigen(x, only.values = TRUE)$values
  all(eigenvalues >= tol)
}
