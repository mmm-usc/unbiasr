#' @importFrom stats qchisq pnorm qnorm nlminb
#' @importFrom mnormt pmnorm
NULL

#' Evaluating selection accuracy based on the MCAA Framework
#'
#' \code{PartInv, PartInvMulti_we} evaluate partial measurement invariance using
#' an extension of Millsap & Kwok's (2004) approach
#'
#' @param propsel Proportion of selection. If missing, computed using `cut_z`.
#' @param cut_z Pre-specified cutoff score on the observed composite. This
#'     argument is ignored when `propsel` has input.
#' @param weights_item A vector of item weights.
#' @param weights_latent A vector of latent factor weights.
#' @param alpha_r A vector of latent factor means for the reference group.
#' @param alpha_f (optional) A vector of latent factor means for the focal group;
#'     if no input, set equal to `alpha_r`.
#' @param psi_r A matrix of latent factor variance-covariances for the
#'     reference group.
#' @param psi_f (optional) A matrix of latent factor variance-covariances for
#'     the focal group; if no input, set equal to `psi_r`.
#' @param lambda_r A matrix of factor loadings for the reference group.
#' @param lambda_f (optional) a matrix of factor loadings for the focal group;
#'     if no input, set equal to `lambda_r`.
#' @param nu_r A matrix of measurement intercepts for the reference group.
#' @param nu_f (optional) A matrix of measurement intercepts for the focal
#'     group; if no input, set equal to `nu_r`.
#' @param Theta_r A matrix of the unique factor variances and covariances
#'     for the reference group.
#' @param Theta_f (optional) A matrix of the unique factor variances and
#'     covariances for the focal group; if no input, set equal to `Theta_r`.
#' @param pmix_ref Proportion of the reference group; default to 0.5 (i.e., two
#'     populations have equal size).
#' @param plot_contour Logical; whether the contour of the two populations
#'     should be plotted; default to `TRUE`.
#' @param show_mi_result If \code{TRUE}, perform selection accuracy analysis
#'     for both the input parameters and the implied parameters based on a
#'     strict invariance model, with common parameter values as weighted
#'     averages of the input values using `pmix_ref`.
#' @param labels A character vector with two elements to label the reference
#'     and the focal group on the graph.
#' @param ... Other arguments passed to the \code{\link[graphics]{contour}}
#'     function.
#' @param phi_r,phi_f,tau_r,tau_f,kappa_r,kappa_f Deprecated; included
#'     only for backward compatibility.
#' @return The output will be a list of four elements and a plot if
#'     \code{plot_contour == TRUE}:
#'         \item{propsel}{Echo the same argument as input.}
#'         \item{cutpt_xi}{Cut point on the latent scale (xi).}
#'         \item{cutpt_z}{Cut point on the observed scale (Z).}
#'         \item{summary}{A 8 x 3 table, with columns representing the reference,
#'             the focal, and the expected results if the latent distribution of
#'             focal group matches the reference group. The rows represent
#'             probabilities of true positive (A), false positive (B),
#'             true negative (C), false negative (D); proportion selected,
#'             success ratio, sensitivity, and specificity.}
#'
#' @examples
#' # Single dimension
#' PartInv(propsel = .30,
#'         weights_item = c(1, 1, 1, 1),
#'         weights_latent = 1,
#'         alpha_r = 0,
#'         alpha_f = 0,
#'         psi_r = 1,
#'         lambda_r = c(1, 1, 1, 1),
#'         nu_r = c(1, 1, 1, 2),
#'         nu_f = c(1, 1, 1, 1),
#'         Theta_r = diag(1, 4),
#'         labels = c("Female", "Male"),
#'         show_mi_result = FALSE)
#'  res <- PartInv(propsel = .50,
#'         alpha_r = 0.5,
#'         alpha_f = 0,
#'         psi_r = 1,
#'         lambda_r = c(1, 1, 1, 1),
#'         nu_r = c(1, 2, 1, 2),
#'         nu_f = c(1, 1, 1, 1),
#'         Theta_r = diag(1, 4),
#'         labels = c("Female", "Male"),
#'         show_mi_result = TRUE)
#' # multiple dimensions
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' PartInv(propsel = .05,
#'         weights_latent = c(0.5, 0.5),
#'         alpha_r = c(0, 0),
#'         alpha_f = c(-0.3, 0.1),
#'         psi_r = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
#'         lambda_r = lambda_matrix,
#'         nu_r = c(.225, .025, .010, .240, .125),
#'         nu_f = c(.225, -.05, .240, -.025, .125),
#'         Theta_r = diag(1, 5),
#'         Theta_f = c(1, .95, .80, .75, 1))
#' PartInvMulti_we(propsel = .10,
#'                 weights_item = c(1/3, 1/3, 1/3, 1/3),
#'                 weights_latent = 1,
#'                 alpha_r = 0.5,
#'                 alpha_f = 0,
#'                 psi_r = 1,
#'                 lambda_r = c(.3, .5, .9, .7),
#'                 nu_r = c(.225, .025, .010, .240),
#'                 nu_f = c(.225, -.05, .240, -.025),
#'                 Theta_r = diag(.96, 4),
#'                 labels = c("female", "male"),
#'                 show_mi_result = TRUE)
#' @export
PartInvMulti_we <- function(propsel, cut_z = NULL,
                            weights_item = NULL,
                            weights_latent = NULL,
                            kappa_r = NULL, kappa_f = kappa_r,
                            alpha_r, alpha_f = alpha_r,
                            phi_r = NULL, phi_f = phi_r,
                            psi_r, psi_f = psi_r,
                            lambda_r, lambda_f = lambda_r,
                            tau_r = NULL, tau_f = tau_r,
                            nu_r, nu_f = nu_r,
                            Theta_r, Theta_f = Theta_r,
                            pmix_ref = 0.5, plot_contour = FALSE,
                            show_mi_result = FALSE,
                            labels = c("Reference", "Focal"), ...) {
  # For backward compatibility with different input names
  if (missing(nu_r) && !is.null(tau_r)) {
    nu_r <- tau_r
    nu_f <- tau_f
  }
  if (missing(alpha_r) && !is.null(kappa_r)) {
    alpha_r <- kappa_r
    alpha_f <- kappa_f
  }
  if (missing(psi_r) && !is.null(phi_r)) {
    psi_r <- phi_r
    psi_f <- phi_f
  }
  if (is.vector(Theta_r)) Theta_r <- diag(Theta_r)
  if (is.vector(Theta_f)) Theta_f <- diag(Theta_f)
  if (is.null(weights_item)) weights_item <- rep(1, length(nu_r))
  if (is.null(weights_latent)) weights_latent <- rep(1, length(alpha_r))
  # convert scalars/vectors to matrices
  alpha_r <- as.matrix(alpha_r)
  alpha_f <- as.matrix(alpha_f)
  psi_r <- as.matrix(psi_r)
  psi_f <- as.matrix(psi_f)
  # check the dimensions of input parameters
  stopifnot(nrow(alpha_r) == ncol(as.matrix(lambda_r)),
            nrow(psi_r) == ncol(as.matrix(lambda_r)))
  mean_zr <- c(crossprod(weights_item, nu_r + lambda_r %*% alpha_r))
  mean_zf <- c(crossprod(weights_item, nu_f + lambda_f %*% alpha_f))
  sd_zr <- c(sqrt(crossprod(weights_item,
                            lambda_r %*% psi_r %*% t(lambda_r) + Theta_r) %*%
                    weights_item))
  sd_zf <- c(sqrt(crossprod(weights_item,
                            lambda_f %*% psi_f %*% t(lambda_f) + Theta_f) %*%
                    weights_item))
  cov_z_xir <- c(crossprod(weights_item, lambda_r %*% psi_r) %*% weights_latent)
  cov_z_xif <- c(crossprod(weights_item, lambda_f %*% psi_f) %*% weights_latent)
  sd_xir <- c(sqrt(crossprod(weights_latent, psi_r) %*% weights_latent))
  sd_xif <- c(sqrt(crossprod(weights_latent, psi_f) %*% weights_latent))
  zeta_r <- c(crossprod(weights_latent, alpha_r))
  zeta_f <- c(crossprod(weights_latent, alpha_f))
  # if there is an input for selection proportion
  if (!missing(propsel)) {
    # and if there is an input for cut score
    if (!is.null(cut_z)) {
      warning("Input to `cut_z` is ignored.")
    }
    # compute the cut score using helper function qnormmix based on input selection
    # proportion
    fixed_cut_z <- FALSE
    cut_z <- qnormmix(propsel, mean_zr, sd_zr, mean_zf, sd_zf,
                      pmix_ref, lower.tail = FALSE)
  } else if (!is.null(cut_z) & missing(propsel)) {
    # if missing selection proportion but has a cut score
    fixed_cut_z <- TRUE
    propsel <- pnormmix(cut_z, mean_zr, sd_zr, mean_zf, sd_zf,
                        pmix_ref, lower.tail = FALSE)
  }
  cut_xi <- qnormmix(propsel, zeta_r, sd_xir, zeta_f, sd_xif,
                     pmix_ref, lower.tail = FALSE)
  # print warning message if propsel is too small
  if (propsel <= 0.01) {
    warning("Proportion selected is 1% or less.")
  }
  # computing summary statistics using helper function .partit_bvnorm
  partit_1 <- .partit_bvnorm(cut_xi, cut_z, zeta_r, sd_xir, mean_zr, sd_zr,
                             cov12 = cov_z_xir)
  partit_2 <- .partit_bvnorm(cut_xi, cut_z, zeta_f, sd_xif, mean_zf, sd_zf,
                             cov12 = cov_z_xif)
  # selection indices for the focal group if latent dist matches the reference
  mean_zref <- c(crossprod(weights_item, nu_f + lambda_f %*% alpha_r))
  sd_zref <- c(sqrt(crossprod(weights_item,
                              lambda_f %*% psi_r %*% t(lambda_f) + Theta_f) %*%
                      weights_item))
  cov_z_xiref <- c(crossprod(weights_item, lambda_f %*% psi_r) %*%
                     weights_latent)
  partit_1e2 <- .partit_bvnorm(cut_xi, cut_z,
                               zeta_r, sd_xir, mean_zref,
                               sd_zref,
                               cov12 = cov_z_xiref)
  zf_par <- list(
    zeta_r = zeta_r,
    zeta_f = zeta_f,
    sd_xir = sd_xir,
    sd_xif = sd_xif,
    mean_zr = mean_zr,
    mean_zf = mean_zf,
    sd_zr = sd_zr,
    sd_zf = sd_zf,
    cov_z_xir = cov_z_xir,
    cov_z_xif = cov_z_xif
  )
  # result table
  dat <- data.frame("Reference" = partit_1, "Focal" = partit_2,
                    "E_R(Focal)" = partit_1e2,
                    row.names = c("A (true positive)", "B (false positive)",
                                  "C (true negative)", "D (false negative)",
                                  "Proportion selected", "Success ratio",
                                  "Sensitivity", "Specificity"))
  colnames(dat) <- c(labels, paste0("E_R(", labels[2], ")"))

  out <- list(propsel = propsel, cutpt_xi = cut_xi, cutpt_z = cut_z,
              summary = dat,
              bivar_data = zf_par,
              ai_ratio = dat["Proportion selected", 3] /
                dat["Proportion selected", 1])

  if (show_mi_result) {  # Need to be updated
    # Strict
    pop_weights <- c(pmix_ref, 1 - pmix_ref)
    lambda_r <- lambda_f <-
      .weighted_average_list(list(lambda_r, lambda_f),
                             weights = pop_weights)
    nu_r <- nu_f <-
      .weighted_average_list(list(nu_r, nu_f),
                             weights = pop_weights)
    Theta_r <- Theta_f <-
      .weighted_average_list(list(Theta_r, Theta_f),
                             weights = pop_weights)
    mean_zr <- c(crossprod(weights_item, nu_r + lambda_r %*% alpha_r))
    mean_zf <- c(crossprod(weights_item, nu_f + lambda_f %*% alpha_f))
    sd_zr <- c(sqrt(crossprod(weights_item,
                              lambda_r %*% psi_r %*% t(lambda_r) + Theta_r) %*%
                      weights_item))
    sd_zf <- c(sqrt(crossprod(weights_item,
                              lambda_f %*% psi_f %*% t(lambda_f) + Theta_f) %*%
                      weights_item))
    cov_z_xir <- c(crossprod(weights_item, lambda_r %*% psi_r) %*% weights_latent)
    cov_z_xif <- c(crossprod(weights_item, lambda_f %*% psi_f) %*% weights_latent)
    sd_xir <- c(sqrt(crossprod(weights_latent, psi_r) %*% weights_latent))
    sd_xif <- c(sqrt(crossprod(weights_latent, psi_f) %*% weights_latent))
    zeta_r <- c(crossprod(weights_latent, alpha_r))
    zeta_f <- c(crossprod(weights_latent, alpha_f))
    if (fixed_cut_z) {
      propsel <- pnormmix(cut_z, mean_zr, sd_zr, mean_zf, sd_zf,
                          pmix_ref, lower.tail = FALSE)
    } else {
      cut_z <- qnormmix(propsel, mean_zr, sd_zr, mean_zf, sd_zf,
                        pmix_ref, lower.tail = FALSE)
    }
    cut_xi <- qnormmix(propsel, zeta_r, sd_xir, zeta_f, sd_xif,
                       pmix_ref, lower.tail = FALSE)
    zf_par_mi <- list(
      zeta_r = zeta_r,
      zeta_f = zeta_f,
      sd_xir = sd_xir,
      sd_xif = sd_xif,
      mean_zr = mean_zr,
      mean_zf = mean_zf,
      sd_zr = sd_zr,
      sd_zf = sd_zf,
      cov_z_xir = cov_z_xir,
      cov_z_xif = cov_z_xif
    )
    partit_1 <- .partit_bvnorm(cut_xi, cut_z, zeta_r, sd_xir, mean_zr, sd_zr,
                               cov12 = cov_z_xir)
    partit_2 <- .partit_bvnorm(cut_xi, cut_z, zeta_f, sd_xif, mean_zf, sd_zf,
                               cov12 = cov_z_xif)
    dat <- data.frame("Reference" = partit_1, "Focal" = partit_2,
                      row.names = c("A (true positive)", "B (false positive)",
                                    "C (true negative)", "D (false negative)",
                                    "Proportion selected", "Success ratio",
                                    "Sensitivity", "Specificity"))
    colnames(dat) <- labels
    out$propsel_mi <- propsel
    out$cutpt_xi_mi <- cut_xi
    out$cutpt_z_mi <- cut_z
    out$summary_mi <- dat
    out$bivar_data_mi <- zf_par_mi
  }
  class(out) <- c("PartInv", "PartInvSummary")
  if (plot_contour) {
    plot(out, labels = labels, ...)
  }
  out
}

#' @rdname PartInvMulti_we
#' @export
PartInv <- PartInvMulti_we
