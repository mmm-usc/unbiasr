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
#' @param labels a character vector with two elements to label the reference
#'            and the focal group on the graph.
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
PartInv_old <- function(propsel, cut_z = NULL, kappa_r, kappa_f = kappa_r, 
                        phi_r, phi_f = phi_r, lambda_r, lambda_f = lambda_r, 
                        Theta_r, Theta_f = Theta_r, tau_r, tau_f = tau_r, 
                        pmix_ref = 0.5, plot_contour = TRUE, 
                        labels = c("Reference group", "Focal group"), ...) {
  .Deprecated("PartInv")
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
