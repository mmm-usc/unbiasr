#' @import pkgload mnormt rsconnect shiny shinydashboard shinyjs shinyMatrix shinyWidgets roxygen2
#' @import shinytest testthat

pnormmix <- function(q, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, pmix1 = 0.5, 
                     lower.tail = TRUE) {
  # Distribution function (pdf) of a mixture of two normal distributions. 
  #
  # Args:
  #   q: vector of quantiles.
  #   mean1: mean of first normal distribution.
  #   sd1: standard deviation of first normal distribution.
  #   mean2: mean of second normal distribution.
  #   sd2: standard deviation of second normal distribution.
  #   pmix1: mixing proportion for the first distribution. Should be a 
  #          number in the range (0, 1).
  #   lower.tail: logical; if TRUE(default), probabilities are P[X <= x]; 
  #               otherwise, P[X > x]. 
  #
  #   Returns:
  #     Cumulative probability of q or 1 minus it on the mixture normal 
  #     distribution.
  # Error handling
  stopifnot(pmix1 > 0, pmix1 < 1)
  as.vector(c(pmix1, 1 - pmix1) %*% 
              sapply(q, pnorm, mean = c(mean1, mean2), sd = c(sd1, sd2), 
                     lower.tail = lower.tail))
}

# test
 pnormmix(1, 0, 3.1, 1.7, 3.1, lower.tail = FALSE)

qnormmix <- function(p, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, pmix1 = 0.5, 
                     lower.tail = TRUE) {
  # Quantile function of a mixture of two normal distributions. 
  #
  # Args:
  #   p: vector of probabilities.
  #   mean1: mean of first normal distribution.
  #   sd1: standard deviation of first normal distribution.
  #   mean2: mean of second normal distribution.
  #   sd2: standard deviation of second normal distribution.
  #   pmix1: mixing proportion for the first distribution. Should be a 
  #          number in the range (0, 1).
  #   lower.tail: logical; if TRUE(default), probabilities are P[X <= x]; 
  #               otherwise, P[X > x]. 
  #
  #   Returns:
  #     Quantile corresponding to p or 1 - p on the mixture normal 
  #     distribution.
  # Error handling
  stopifnot(pmix1 > 0, pmix1 < 1, p >= 0, p <= 1)
  f <- function(x) (pnormmix(x, mean1, sd1, mean2, sd2, pmix1, 
                             lower.tail) - p)^2
  start <- as.vector(c(pmix1, 1 - pmix1) %*% 
                       sapply(p, qnorm, c(mean1, mean2), c(sd1, sd2), 
                              lower.tail = lower.tail))
  nlminb(start, f)$par
}

.bvnorm_kernel <- function(x, y, mu_x = 0, mu_y = 0, sd_x = 1, sd_y = 1, 
                           cov_xy = 0) {
  # Helper funcction for computing the kernel for bivariate normal density
  cor <- cov_xy / sd_x / sd_y
  numer <- (x - mu_x)^2 / sd_x^2 + (y - mu_y)^2 / sd_y^2 - 
    2 * cor * (x - mu_x) * (y - mu_y) / sd_x / sd_y
  numer / (1 - cor^2)
}

contour_bvnorm <- function(mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, 
                           cor12 = 0, cov12 = NULL, 
                           density = .95, length_out = 101, 
                           bty = "L", 
                           ...) {
  # Plot contour for a bivariate normal distribution
  #
  # Args:
  #   mean1: mean of first normal distribution (on x-axis).
  #   sd1: standard deviation of first normal distribution.
  #   mean2: mean of second normal distribution (on y-axis).
  #   sd2: standard deviation of second normal distribution.
  #   cor12: correlation in the bivariate normal.
  #   cov12: covariance in the bivariate normal. If not input, compute the
  #          covariance using the correlation and the standard deviations.
  #   density: density level, i.e., probability enclosed by the ellipse.
  #   length_out: number of values on the x-axis and on the y-axis to be
  #               evaluated; default to 101.
  #   bty: argument passed to the `contour` function.
  #   ...: other arguments passed to the `countour` funcction.
  #
  #   Returns:
  #     a plot showing the contour of the bivariate normal distribution on 
  #     a two-dimensional space.
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

.partit_bvnorm <- function(cut1, cut2, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, 
                           cor12 = 0, cov12 = cor12 * sd1 * sd2) {
  # Helper funcction for computing summary statistics from a selection approach
  Sigma <- matrix(c(sd1^2, cov12, cov12, sd2^2), nrow = 2)
  C <- pmnorm(c(cut1, cut2), c(mean1, mean2), Sigma)
  B <- pnorm(cut1, mean1, sd1) - C
  D <- pnorm(cut2, mean2, sd2) - C
  A <- 1 - B - C - D
  propsel <- A + B
  success_ratio <- A / propsel
  sensitivity <- A / (A + D)
  specificity <- C / (C + B)
  c(A, B, C, D, propsel, success_ratio, sensitivity, specificity)
}

#' Evaluate partial measurement invariance using Millsap & Kwok's (2004) 
#' approach
#' 
#' @param propsel proportion of selection. If missing, computed using `cut_z`.
#' @param cut_z prespecified cutoff score on the observed composite.
#'              This argument is ignored when `propsel` has input.
#' @param kappa_r latent factor mean for the reference group.
#' @param kappa_f (optional) latent factor mean for the focal group; 
#'                if no input, set equal to `kappa_r`.
#' @param phi_r latent factor variance for the reference group.
#' @param phi_f (optional) latent factor variance for the focal group; 
#'              if no input, set equal to `phi_r`.
#' @param lambda_r a vector of factor loadings for the reference group.
#' @param lambda_f (optional) a vector of factor loadings for the focal group; 
#'                 if no input, set equal to `lambda_r`.
#' @param tau_r a vector of measurement intercepts for the reference group.
#' @param tau_f (optional) a vector of measurement intercepts for the focal group; 
#'              if no input, set equal to `tau_r`.
#' @param Theta_r a matrix of the unique factor variances and covariances 
#'                for the reference group.
#' @param Theta_f (optional) a matrix of the unique factor variances and 
#'                covariances for the focal group; if no input, set equal to 
#'                `Theta_r`.
#' @param pmix_ref Proportion of the reference group; default to 0.5 
#'                 (i.e., two populations have equal size).
#' @param plot_contour logical; whether the contour of the two populations 
#'                     should be plotted; default to `TRUE`.
#' @param ... other arguments passed to the \code{\link[graphics]{contour}} 
#'            function.
#' @return a list of four elements and a plot if \code{plot_contour == TRUE}.
#'   The four elements are
#' \describe{
#'   \item{propsel}{echo the same argument as input}
#'   \item{cutpt_xi}{cut point on the latent scale (xi)}
#'   \item{cutpt_z}{cut point on the observed scale (Z)}
#'   \item{summary}{A 8 x 2 table, with columns representing the reference 
#'                  and the focal groups, and the rows represent probabilities
#'                  of true positive (A), false positive (B), 
#'                  true negative (C), false negative (D); proportion selected, 
#'                  success ratio, sensitivity, and specificity. }
#' }
#' @examples
#' PartInv(.25, kappa_r = 0.5, kappa_f = 0, phi_r = 1,
#'         lambda_r = c(.3, .5, .9, .7), tau_r = c(.225, .025, .010, .240),
#'         Theta_r = diag(.96, 4), labels = c("female", "male"))
#' @export
PartInv <- function(propsel, cut_z = NULL, kappa_r, kappa_f = kappa_r, 
                    phi_r, phi_f = phi_r, lambda_r, lambda_f = lambda_r, 
                    Theta_r, Theta_f = Theta_r, tau_r, tau_f = tau_r, 
                    pmix_ref = 0.5, plot_contour = TRUE, 
                    labels = c("Reference group", "Focal group"), ...) {
  # Error handling
  stopifnot(length(kappa_r) == 1, length(kappa_f) == 1, length(phi_r) == 1, 
            length(phi_f) == 1)
  if (length(Theta_r) == length(lambda_r)) Theta_r <- diag(Theta_r)
  if (length(Theta_f) == length(lambda_f)) Theta_f <- diag(Theta_f)
  # Convert 1x1 matrices to vector
  kappa_r <- c(kappa_r)
  kappa_f <- c(kappa_f)
  phi_r <- c(phi_r)
  phi_f <- c(phi_f)
  #library(mnormt)  # load `mnormt` package
  mean_zr <- sum(tau_r) + sum(lambda_r) * kappa_r
  mean_zf <- sum(tau_f) + sum(lambda_f) * kappa_f
  sd_zr <- sqrt(sum(lambda_r)^2 * phi_r + sum(Theta_r))
  sd_zf <- sqrt(sum(lambda_f)^2 * phi_f + sum(Theta_f))
  cov_z_xir <- sum(lambda_r) * phi_r
  cov_z_xif <- sum(lambda_f) * phi_f
  sd_xir <- sqrt(phi_r)
  sd_xif <- sqrt(phi_f)
  if (!missing(propsel)) {
    if (!is.null(cut_z)) {
      warning("Input to `cut_z` is ignored.")
    }
    cut_z <- qnormmix(propsel, mean_zr, sd_zr, mean_zf, sd_zf, 
                      pmix_ref, lower.tail = FALSE)
  } else if (!is.null(cut_z) & missing(propsel)) {
    propsel <- pnormmix(cut_z, mean_zr, sd_zr, mean_zf, sd_zf, 
                        pmix_ref, lower.tail = FALSE)
  }
  cut_xi <- qnormmix(propsel, kappa_r, sd_xir, kappa_f, sd_xif, 
                     pmix_ref, lower.tail = FALSE)
  partit_1 <- .partit_bvnorm(cut_xi, cut_z, kappa_r, sd_xir, mean_zr, sd_zr, 
                             cov12 = cov_z_xir)
  partit_2 <- .partit_bvnorm(cut_xi, cut_z, kappa_f, sd_xif, mean_zf, sd_zf, 
                             cov12 = cov_z_xif)
  dat <- data.frame("Reference" = partit_1, "Focal" = partit_2, 
                    row.names = c("A (true positive)", "B (false positive)", 
                                  "C (true negative)", "D (false negative)", 
                                  "Proportion selected", "Success ratio", 
                                  "Sensitivity", "Specificity")
                   )
  colnames(dat) <- labels
  p <- NULL
  if (plot_contour) {
    x_lim <- range(c(kappa_r + c(-3, 3) * sd_xir, 
                     kappa_f + c(-3, 3) * sd_xif))
    y_lim <- range(c(mean_zr + c(-3, 3) * sd_zr, 
                     mean_zf + c(-3, 3) * sd_zf))
    contour_bvnorm(kappa_r, sd_xir, mean_zr, sd_zr, cov12 = cov_z_xir, 
                   xlab = bquote("Latent Score" ~ (xi)), 
                   ylab = bquote("Observed Composite" ~ (italic(Z))), 
                   lwd = 2, col = "red", xlim = x_lim, ylim = y_lim, 
                   ...)
    contour_bvnorm(kappa_f, sd_xif, mean_zf, sd_zf, cov12 = cov_z_xif, 
                   add = TRUE, lty = "dashed", lwd = 2, col = "blue", 
                   ...)
    legend("topleft", labels,
           lty = c("solid", "dashed"), col = c("red", "blue"))
    abline(h = cut_z, v = cut_xi)
    x_cord <- rep(cut_xi + c(.25, -.25) * sd_xir, 2)
    y_cord <- rep(cut_z + c(.25, -.25) * sd_zr, each = 2)
    text(x_cord, y_cord, c("A", "B", "D", "C"))
    p <- recordPlot()
  }
  list(propsel = propsel, cutpt_xi = cut_xi, cutpt_z = cut_z, 
       summary = round(dat, 3), p = p)
}
