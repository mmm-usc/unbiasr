#' @importFrom graphics legend abline text contour
NULL

#' Plot contour for a bivariate normal distribution
#' 
#' @param mean1 Mean of the first normal distribution (on x-axis).
#' @param sd1 Standard deviation of thefirst normal distribution.
#' @param mean2 Mean of the second normal distribution (on y-axis).
#' @param sd2 Standard deviation of the second normal distribution.
#' @param cor12 Correlation in the bivariate normal distribution.
#' @param cov12 Covariance in the bivariate normal distribution. If not input,
#'     compute the covariance using the correlation and the standard deviations.
#' @param density Density level, i.e., probability enclosed by the ellipse.
#' @param length_out Number of values on the x-axis and on the y-axis to be
#'               evaluated; default to 101.
#' @param bty Argument passed to the `contour` function.
#' @param ... Additional arguments passed to \code{\link[graphics]{contour}}
#'
#' @return A plot showing the contour of the bivariate normal distribution on
#'     a two-dimensional space.
#' @examples
#' \dontrun{
#' contour_bvnorm(
#'   0.5, 1, 0.57, 1.03, cov12 = 0.8,
#'   xlab = bquote("Latent Composite" ~ (zeta)),
#'   ylab = bquote("Observed Composite" ~ (italic(Z))),
#'   lwd = 2, col = "red", xlim = c(-3.0, 3.5),
#'   ylim = c(-2.97, 3.67)
#' )
#' }

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

#'@export
plot.PartInv <- function(x, labels,
                         which_result = c("pi", "mi"), ...) {
    which_result <- match.arg(which_result)
    if (which_result == "pi") {
        plot_dat <- x$bivar_data
        cut_xi <- x$cutpt_xi
        cut_z <- x$cutpt_z
        summ <- x$summary
    } else if (which_result == "mi") {
        summ <- x$summary_mi
        if (is.null(summ)) {
            stop("Strict invariance results not found. ",
                 "Please include `show_mi_result = TRUE` ",
                 "when running `PartInv()`")
        }
        plot_dat <- x$bivar_data_mi
        cut_xi <- x$cutpt_xi_mi
        cut_z <- x$cutpt_z_mi
    }
    if (missing(labels)) {
        labels <- colnames(summ)[1:2]
    }
    x_lim <- range(c(plot_dat$zeta_r + c(-3, 3) * plot_dat$sd_xir,
                     plot_dat$zeta_f + c(-3, 3) * plot_dat$sd_xif))
    y_lim <- range(c(plot_dat$mean_zr + c(-3, 3) * plot_dat$sd_zr,
                     plot_dat$mean_zf + c(-3, 3) * plot_dat$sd_zf))
    contour_bvnorm(plot_dat$zeta_r, plot_dat$sd_xir,
                   plot_dat$mean_zr, plot_dat$sd_zr,
                   cov12 = plot_dat$cov_z_xir,
                   xlab = bquote("Latent Composite" ~ (zeta)),
                   ylab = bquote("Observed Composite" ~ (italic(Z))),
                   lwd = 2, col = "red", xlim = x_lim, ylim = y_lim,
                   ...)
    contour_bvnorm(plot_dat$zeta_f, plot_dat$sd_xif,
                   plot_dat$mean_zf, plot_dat$sd_zf,
                   cov12 = plot_dat$cov_z_xif,
                   add = TRUE, lty = "dashed", lwd = 2, col = "blue",
                   ...)
    legend("topleft", labels,
           lty = c("solid", "dashed"), col = c("red", "blue"))
    abline(h = cut_z, v = cut_xi)
    x_cord <- rep(cut_xi + c(.25, -.25) * plot_dat$sd_xir, 2)
    y_cord <- rep(cut_z + c(.25, -.25) * plot_dat$sd_zr, each = 2)
    text(x_cord, y_cord, c("A", "B", "D", "C"))
}
