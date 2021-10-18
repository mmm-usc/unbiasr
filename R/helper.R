#' Distribution function (pdf) of a mixture of two normal distributions. 
#' \code{pnormmix} returns the cumulative probability of q or \eqn{1 - q} on the
#'  mixture normal distribution.
#' 
#' @param q A vector of quantiles.
#' @param mean1: Mean of the first normal distribution.
#' @param sd1: Standard deviation of the first normal distribution.
#' @param mean2: Mean of the second normal distribution.
#' @param sd2: Standard deviation of the second normal distribution.
#' @param pmix1: Mixing proportion for the first distribution. Should be a 
#'   number in the range (0, 1).
#' @param lower.tail: A logical scalar; if TRUE(default), probabilities are 
#' \eqn{P[X <= x]}; otherwise, \eqn{P[X > x]}. 
#' @return The output will be the cumulative probability of q or \eqn{1 - q} on 
#' the mixture normal distribution.
#' @examples
#' pnormmix(1, 0, 3.1, 1.7, 3.1, lower.tail = FALSE)
#' 

pnormmix <- function(q, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, pmix1 = 0.5, 
                     lower.tail = TRUE) {
  stopifnot(pmix1 > 0, pmix1 < 1)
  as.vector(c(pmix1, 1 - pmix1) %*% 
              sapply(q, pnorm, mean = c(mean1, mean2), sd = c(sd1, sd2), 
                     lower.tail = lower.tail))
}

# pnormmix(, mean1 = 1.53, sd1 = 0.89, mean2 = 1.32, sd2 = 0.88, pmix1 = 0.5, lower.tail = FALSE)

#' Quantile function of a mixture of two normal distributions. 
#' \code{qnormmix} returns the quantile corresponding to p or \eqn{1 - q} on 
#' the mixture normal distribution.
#' 
#' @param p A vector of probabilities.
#' @param mean1: Mean of the first normal distribution.
#' @param sd1: Standard deviation of the first normal distribution.
#' @param mean2: Mean of the second normal distribution.
#' @param sd2: Standard deviation of the second normal distribution.
#' @param pmix1: Mixing proportion for the first distribution. Should be a 
#'   number in the range (0, 1).
#' @param lower.tail: A logical scalar; if TRUE(default), probabilities are 
#' \eqn{P[X <= x]}; otherwise, \eqn{P[X > x]}. 
#' @return The output will be the quantile corresponding to p or \eqn{1 - q} on 
#' the mixture normal distribution.
#' @examples
#' qnormmix(0.8, 0, 3.1, 1.7, 0.5, lower.tail = FALSE)
#' 

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
#' \code{.bvnorm_kernel} returns the kernel for bivariate normal density
#' 
#' @param x A normal distribution
#' @param mu_x: Mean of the normal distribution x.
#' @param y A normal distribution
#' @param sd_x: Standard deviation of the normal distribution x.
#' @param mu_y: Mean of the normal distribution y.
#' @param sd_y: Standard deviation of the normal distribution y.
#' @param cov_xy: covariance between x and y
#' 
#' @return The output will be the quantile corresponding to p or \eqn{1 - q} on 
#' the mixture normal distribution.
#' @examples
#' 

.bvnorm_kernel <- function(x, y, mu_x = 0, mu_y = 0, sd_x = 1, sd_y = 1, 
                           cov_xy = 0) {
  
  cor <- cov_xy / sd_x / sd_y
  numer <- (x - mu_x)^2 / sd_x^2 + (y - mu_y)^2 / sd_y^2 - 
    2 * cor * (x - mu_x) * (y - mu_y) / sd_x / sd_y
  numer / (1 - cor^2)
}

#' Plot contour for a bivariate normal distribution
#' \code{.bvnorm_kernel} returns the kernel for bivariate normal density
#' 
#' @param mean1: Mean of first normal distribution (on x-axis).
#' @param sd1: Standard deviation of first normal distribution.
#' @param mean2: Mean of second normal distribution (on y-axis).
#' @param sd2: Standard deviation of second normal distribution.
#' @param cor12: Correlation in the bivariate normal.
#' @param cov12: Covariance in the bivariate normal. If not input, compute the
#'          covariance using the correlation and the standard deviations.
#' @param density: Density level, i.e., probability enclosed by the ellipse.
#' @param length_out: Number of values on the x-axis and on the y-axis to be
#'               evaluated; default to 101.
#' @param bty: Argument passed to the `contour` function.
#' 
#' @return A plot showing the contour of the bivariate normal distribution on 
#'   a two-dimensional space.
#' @examples 
#' 
#'  

contour_bvnorm <- function(mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, 
                           cor12 = 0, cov12 = NULL, 
                           density = .95, length_out = 101, 
                           bty = "L", 
                           ...) {
  # Error handling
  stopifnot(cor12 >= -1, cor12 <= 1)
  if (is.null(cov12)) cov12 <- cor12 * sd1 * sd2
  x_seq <- mean1 + seq(-3, 3, length.out = length_out) * sd1
  y_seq <- mean2 + seq(-3, 3, length.out = length_out) * sd2
  z <- outer(x_seq, y_seq, .bvnorm_kernel, mu_x = mean1, mu_y = mean2, 
             sd_x = sd1, sd_y = sd2, cov_xy = cov12)
  contour(x_seq, y_seq, z, levels = qchisq(density, 2), drawlabels = FALSE, 
          bty = bty, ...)
}

#' Computing summary statistics from a selection approach
#' \code{.partit_bvnorm} returns a table of selection accuracy indices
#' 
#' @param cut1: Cut score based on the latent score
#' @param cut2: Cut score based on the observed score
#' @param mean1: Mean of first normal distribution (on x-axis).
#' @param sd1: Standard deviation of first normal distribution.
#' @param mean2: Mean of second normal distribution (on y-axis).
#' @param sd2: Standard deviation of second normal distribution.
#' @param cor12: Correlation in the bivariate normal.
#' @param cov12: Covariance in the bivariate normal. If not input, compute the
#'          covariance using the correlation and the standard deviations.
#' 
#' @return A table of selection accuracy indices
#' @examples 
#' 
#'  

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

# .partit_bvnorm(cut1 = 8.45, cut2 = 9, mean1 = 0, sd1 = 1, mean2 = 1.53, sd2 = 0.89,
#                cov12 = 0.80)
