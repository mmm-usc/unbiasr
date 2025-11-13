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
#'   compute the covariance using the correlation and the standard deviations.
#' @param density Density level, i.e., probability enclosed by the ellipse.
#' @param length_out Number of values on the x-axis and on the y-axis to be
#'   evaluated; default to 101.
#' @param bty Argument passed to the `contour` function.
#' @param ... Additional arguments passed to \code{\link[graphics]{contour}}
#'
#' @return A plot showing the contour of the bivariate normal distribution on
#'   a two-dimensional space.
#' @examples
#' \dontrun{
#' contour_bvnorm(
#'   0.5, 1, 0.57, 1.03, cov12 = 0.8,
#'   xlab = bquote("Latent Composite" ~ (eta)),
#'   ylab = bquote("Observed Composite" ~ (italic(Z))),
#'   lwd = 2, col = "red", xlim = c(-3.0, 3.5),
#'   ylim = c(-2.97, 3.67)
#' )
#' }
contour_bvnorm <- function(mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1,
                           cor12 = 0, cov12 = NULL,
                           density = .95, length_out = 101,
                           bty = "L", ...) {
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

#' Contour plots for multiple groups.
#' 
#' Plotting method for objects of class [`PartInv`] to show the contours for
#' any number of groups.
#' 
#' @param x PartInv output.
#' @param labels Character vector. By default, use the `labels` value from
#'   `x`.
#' @param which_result Character; whether to operate on the partial (`"mi"`)
#'   or the strict invariance (`"mi"`) plot.
#' @param custom_colors Optional argument for specifying the colors of the 
#'   ellipses. 
#' @param quadrantsABCD Whether to label the quadrants with A, B, C, D or TR,
#'   FP, TN, FN. `TRUE` by default.
#' @param ... Additional arguments.
#'@export
plot.PartInv <- function(x, labels = x[["params"]][["labels"]],
                         which_result = NULL,
                         custom_colors = NULL, 
                         quadrantsABCD = TRUE, 
                         ...) {
  
  valid_results <- c("pi", "mi")
  if (is.null(which_result)) {
    if (length(x$bivar_data) > 0) which_result <- c(which_result, "pi")
    if (length(x$bivar_data_mi) > 0) which_result <- c(which_result, "mi")
    if (length(which_result) == 0) {
      stop("No data available for plotting. Please ensure `x$bivar_data` or `x$bivar_data_mi` is not null.")
    }
  }
  
  # Validate which_result
  if (!all(which_result %in% valid_results)) {
    stop("Invalid value for `which_result`. Choose from 'pi', 'mi', or both.")
  }
    
  # which_result <- match.arg(which_result, valid_results, several.ok = TRUE)
  
  n_g <- length(x$bivar_data$mn_xi) # number of groups
  
  # find the range of limit values under pi and mi conditions before looping
  # through which_results, so that the limits are consistently assigned across
  # pi and mi conditions
  x_lim_mi <- x_lim_pi <- y_lim_mi <- y_lim_pi <- c()
  for (i in seq_len(n_g)) {
    x_lim_pi <- c(x_lim_pi, c(x$bivar_data$mn_xi[i] + c(-3, 3) * x$bivar_data$sd_xi[i]))
    y_lim_pi <- c(y_lim_pi, x$bivar_data$mn_z[i] + c(-3, 3) * x$bivar_data$sd_z[i])
    if(!is.null(x$bivar_data_mi)) {
      x_lim_mi <- c(x_lim_mi, c(x$bivar_data_mi$mn_xi[i] + c(-3, 3) * 
                                  x$bivar_data_mi$sd_xi[i]))
      y_lim_mi <- c(y_lim_mi, x$bivar_data_mi$mn_z[i] + c(-3, 3) * 
                      x$bivar_data_mi$sd_z[i])
    }
  }
  
  x_lim <- range(x_lim_pi, x_lim_mi)
  y_lim <- range(y_lim_pi, y_lim_mi)
  
  for (r in which_result) {
    if (r == "pi") {
        plot_dat <- x$bivar_data
        cut_xi <- x$cutpt_xi
        cut_z <- x$cutpt_z
        summ <- x$summary
        title <- c("Partial Measurement Invariance")
    } else if (r == "mi") {
        summ <- x$summary_mi
        if (is.null(summ)) {
            stop("Strict invariance results not found. ",
                 "Please include `show_mi_result = TRUE` when running `PartInv()`.")
        }
        plot_dat <- x$bivar_data_mi
        cut_xi <- x$cutpt_xi_mi
        cut_z <- x$cutpt_z_mi
        title <- c("Strict Measurement Invariance")
    }
  
    colorlist <- colorlist()
    ltylist <- rep(c('twodash', 'longdash', 'dotdash', 'dashed', 'dotted'), 
                   length.out = n_g)
    if (!is.null(custom_colors)) { colorlist <- custom_colors }

    # Plot the ellipse for the reference group
    contour_bvnorm(plot_dat$mn_xi[1], plot_dat$sd_xi[1],
                   plot_dat$mn_z[1], plot_dat$sd_z[1],
                   cov12 = plot_dat$cov_z_xi[1],
                   xlab = bquote("Latent Composite" ~ (zeta)),
                   ylab = bquote("Observed Composite" ~ (italic(Z))),
                   lwd = 2, col = colorlist[1], xlim = x_lim, ylim = y_lim,
                   main = title)
    # Add on the ellipses for the focal groups
    for (i in 2:n_g) {
      contour_bvnorm(plot_dat$mn_xi[i], plot_dat$sd_xi[i],
                     plot_dat$mn_z[i], plot_dat$sd_z[i],
                     cov12 = plot_dat$cov_z_xi[i],
                     add = TRUE, lwd = 2, col = colorlist[i], 
                     lty = ltylist[i])
    }
    legend("topleft", labels, lty = c("solid", ltylist[2:n_g]), 
           col = colorlist[1:n_g])
    abline(h = cut_z, v = cut_xi)
    x_cord <- rep(cut_xi + c(.8, -.8) * plot_dat$sd_xi[1], 2)
    y_cord <- rep(cut_z + c(.8, -.8) * plot_dat$sd_z[1], each = 2)
    if (quadrantsABCD) { 
      text(x_cord, y_cord, c("A", "B", "D", "C"))
    } else {
      text(x_cord, y_cord, c("TP", "FP", "FN", "TN"))
    }
    
    if (n_g > 20) {
      warning("If you would like to plot the contours of more than 20 groups, 
              please provide a list of 20 color names.")
    }
  }
}