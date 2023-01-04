#' @importFrom stats qchisq pnorm qnorm nlminb
#' @importFrom mnormt pmnorm
#' @import zeallot

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
#' @param lambda_f (optional) A matrix of factor loadings for the focal group;
#'     if no input, set equal to `lambda_r`.
#' @param nu_r A matrix of measurement intercepts for the reference group.
#' @param nu_f (optional) A matrix of measurement intercepts for the focal
#'     group; if no input, set equal to `nu_r`.
#' @param Theta_r A matrix of the unique factor variances and covariances
#'     for the reference group.
#' @param Theta_f (optional) A matrix of the unique factor variances and
#'     covariances for the focal group; if no input, set equal to `Theta_r`.
#' @param pmix Proportion of the reference group; default to 0.5 (i.e., two
#'     populations have equal size).
#' @param plot_contour Logical; whether the contour of the two populations
#'     should be plotted; default to `TRUE`.
#' @param show_mi_result If \code{TRUE}, perform selection accuracy analysis
#'     for both the input parameters and the implied parameters based on a
#'     strict invariance model, with common parameter values as weighted
#'     averages of the input values using `pmix`.
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
#' @export
PartInv_mult <- function(propsel = NULL, cut_z = NULL,
                            weights_item = NULL,
                            weights_latent = NULL,
                            alpha, psi, lambda, nu, Theta,
                            pmix = NULL, plot_contour = FALSE,
                            show_mi_result = FALSE,
                            labels = c("Reference", "Focal"), ...) {
  
  stopifnot("Number of groups as indicated in the lengths of parameters must
            match." = length(alpha) == lengths(list(psi, lambda, nu, Theta)))
  stopifnot("Number of dimensions must match." = 
              any(lengths(alpha) == unlist(lapply(lambda, ncol))))
  stopifnot("Provide the correct number of mixing proportions." = 
              length(pmix) == length(alpha))
  
  num_g <- length(alpha); n <- length(nu[[1]]); d <- length(alpha[[1]])
  
  if(is.null(pmix)) pmix <- as.matrix(c(rep(1/num_g, num_g)), ncol = num_g)
  pmix <- as.vector(pmix)
  
  g <- c("r", paste0("f", 1:(num_g - 1)))

  names(alpha) <- paste("alpha", g, sep = "_")
  names(nu) <- paste("nu", g, sep = "_")
  names(lambda) <- paste("lambda", g, sep = "_")
  names(psi) <- paste("psi", g, sep = "_")
  names(Theta) <- paste("Theta", g, sep = "_")
  
  Theta <- lapply(Theta, diag, nrow = n)
  alpha <- lapply(alpha, as.matrix)
  psi <- lapply(psi, matrix, nrow = d, ncol = d)
  
  if (is.null(weights_item)) weights_item <- rep(1, n)
  if (is.null(weights_latent)) weights_latent <- rep(1, d)

  mn_z <- sd_z <- mn_xi <- sd_xi <- cov_z_xi <- NULL
  c(mn_z, sd_z, mn_xi, sd_xi, cov_z_xi) %<-% 
    mn_sd_cov_mult(num_g, weights_item, weights_latent, alpha, psi, lambda, nu,
                   Theta)
  
  # if there is an input for selection proportion
  if (!is.null(propsel)) {
    if (!is.null(cut_z))  warning("Input to `cut_z` is ignored.")
    
    # compute the cut score using qnormmix based on input selection proportion
    fixed_cut_z <- FALSE
    cut_z <- qnormmix_mult(propsel, means = mn_z, sds = sd_z, pmix = pmix, lower.tail = FALSE)

  } else if (!is.null(cut_z) & is.null(propsel)) {

    # if selection proportion is missing but a cut score was provided
    fixed_cut_z <- TRUE
    # compute the selection proportion using pnormmix based on the cutoff value
    propsel <- pnormmix_mult(cut_z, means = mn_z, sds = sd_z, pmix = pmix, 
                             lower.tail = FALSE)
  }

  # compute the threshold for the latent variable based on the selection 
  # proportion provided by the user/computed using cut_z
  cut_xi <- qnormmix_mult(propsel, means = mn_xi, sds = sd_xi, pmix = pmix, 
                          lower.tail = FALSE)
  
  # print warning message if propsel is too small
  if (propsel <= 0.01) warning("Proportion selected is 1% or less.")
  
 CAIs <- matrix(ncol = num_g + num_g - 1, nrow = 8) 
 
 for (i in 1:num_g) {
   CAIs[,i] <- .partit_bvnorm(cut_xi, cut_z, mn_xi[[i]], sd_xi[[i]],
                              mn_z[[i]], sd_z[[i]], cov12 = cov_z_xi[[i]])
 }
 
 # Store mean, sd, cov values for the obs/latent variables 
 zf_par <- list(mn_xi = mn_xi, sd_xi = sd_xi, mn_z = mn_z, sd_z = sd_z,
                cov_z_xi = cov_z_xi)

 # selection indices for the focal group if its distribution matches the
 # distribution of the reference group (Efocal)
 mn_z_Ef <- sd_z_Ef <- mn_xi_Ef <- sd_xi_Ef <- cov_z_xi_Ef <- CAI_Ef <- c()
 
 for (i in 2:num_g) {
   mn_z_Ef[i - 1] <- c(crossprod(weights_item, nu[[i]] + lambda[[i]]
                                   %*% alpha[[1]]))
   sd_z_Ef[i - 1] <- c(sqrt(crossprod(weights_item, lambda[[i]] %*% psi[[1]]
                                        %*% t(lambda[[i]]) + Theta[[i]])
                              %*% weights_item))
   cov_z_xi_Ef[i - 1] <- c(crossprod(weights_item, lambda[[i]] %*% psi[[1]]) 
                             %*% weights_latent)
 
   CAIs[, i + num_g - 1] <- .partit_bvnorm(cut_xi, cut_z, mn_xi[[1]], sd_xi[[1]],
                                      mn_z_Ef[[i - 1]], sd_z_Ef[[i - 1]],
                                      cov12 = cov_z_xi_Ef[[i - 1]])
 }


 dat <- data.frame(CAIs, row.names = c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP"))
                  # row.names = c("A (true positive)", "B (false positive)",
                  #               "C (true negative)", "D (false negative)",
                  #               "Proportion selected", "Success ratio",
                  #               "Sensitivity", "Specificity"))
 names(dat) <- c("Reference", paste0("Focal_", 1:(num_g - 1)),
                 paste0("E_R(Focal)_", 1:(num_g - 1)))
 
 ai_ratio <-  dat[5, (num_g + 1):(num_g + num_g - 1)] / dat[5, 1]
 names(ai_ratio) <- paste0("Focal_", 1:(num_g - 1))
 row.names(ai_ratio) <- c("")
 out <- list(propsel = propsel, cutpt_xi = cut_xi, cutpt_z = cut_z,
             summary = dat, bivar_data = zf_par, ai_ratio = ai_ratio)
 
 if (show_mi_result) { 
   pop_weights <- pmix
   lambda_average <-
     .weighted_average_list(lambda, weights = pop_weights)
   nu_average <-
     .weighted_average_list(nu, weights = pop_weights)
   Theta_average <- 
     .weighted_average_list(Theta, weights = pop_weights)
   
   #SHOULD WE BE GETTING WEIGHTED AVERAGES FOR PSI, ALPHA AS WELL?
   
   lambda_average_g <- nu_average_g <- Theta_average_g <- vector(mode = "list",
                                                                 length = num_g)
   for (i in 1:num_g) {
     lambda_average_g[[i]] <- lambda_average
     nu_average_g[[i]] <- nu_average
     Theta_average_g[[i]] <- Theta_average
   }
   c(mn_z, sd_z, mn_xi, sd_xi, cov_z_xi) %<-% 
     mn_sd_cov_mult(num_g, weights_item, weights_latent, alpha, psi,
                    lambda_average_g, nu_average_g, Theta_average_g)

   if (fixed_cut_z) {
     propsel <- pnormmix_mult(cut_z, mn_z, sd_z, pmix, lower.tail = FALSE)
   } else {
     cut_z <- qnormmix_mult(propsel, mn_z, sd_z, pmix, lower.tail = FALSE)
   }
   cut_xi <- qnormmix_mult(propsel, mn_xi, sd_xi, pmix, lower.tail = FALSE)

   for (i in 1:num_g) {
     CAIs[ ,i] <- .partit_bvnorm(cut_xi, cut_z, mn_xi[[i]], sd_xi[[i]],
                                mn_z[[i]], sd_z[[i]], cov12 = cov_z_xi[[i]])
   }
   
   # Store mean, sd, cov values for the obs/latent variables 
   zf_par_mi <- list(mn_xi = mn_xi, sd_xi = sd_xi, mn_z = mn_z, sd_z = sd_z,
                  cov_z_xi = cov_z_xi)
   dat <- data.frame(CAIs, row.names = c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP"))
   
   names(dat) <- c("Reference", paste0("Focal_", 1:(num_g - 1)),
                   paste0("E_R(Focal)_", 1:(num_g - 1)))

   out$propsel_mi <- propsel
   out$cutpt_xi_mi <- cut_xi
   out$cutpt_z_mi <- cut_z
   out$summary_mi <- dat
   out$bivar_data_mi <- zf_par_mi
   }
 class(out) <- c('PartInv', 'PartInvSummary')
 
 #if (plot_contour) {
  # plot(out, labels = labels, ...)
 #}
  return(out)
}
