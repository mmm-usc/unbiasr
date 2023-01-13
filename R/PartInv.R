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
#' @param lambda_f (optional) A matrix of factor loadings for the focal group;
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
#' @return The output will be a list of six elements and a plot if
#'     \code{plot_contour == TRUE}:
#'         \item{propsel}{Proportion selected.}
#'         \item{cutpt_xi}{Cut point on the latent scale (xi).}
#'         \item{cutpt_z}{Cut point on the observed scale (Z).}
#'         \item{summary}{A 8 x 3 table, with columns representing the reference,
#'             the focal, and the expected results if the latent distribution of
#'             focal group matches the reference group. The rows represent
#'             probabilities of true positive (A), false positive (B),
#'             true negative (C), false negative (D); proportion selected,
#'             success ratio, sensitivity, and specificity.}
#'         \item{bivardata}{List of length `10` containing scalars of 
#'             latent and observed means, standard deviations, and covariances 
#'             computed for the reference and focal groups.}
#'         \item{ai_ratio}{The AI ratio.}
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
PartInvMulti_we <- function(propsel = NULL, cut_z = NULL,
                            weights_item = NULL,
                            weights_latent = NULL,
                            alpha, psi, lambda, Theta, nu,
                            kappa_r = NULL, kappa_f = kappa_r,
                            alpha_r = NULL, alpha_f = alpha_r,
                            phi_r = NULL, phi_f = phi_r,
                            psi_r = NULL, psi_f = psi_r,
                            lambda_r = NULL, lambda_f = lambda_r,
                            tau_r = NULL, tau_f = tau_r,
                            nu_r = NULL, nu_f = nu_r,
                            Theta_r = NULL, Theta_f = Theta_r,
                            pmix = 0.5,
                            pmix_ref = 0.5, plot_contour = FALSE,
                            show_mi_result = FALSE,
                            labels = c("Reference", "Focal"), ...) {
  
  # for backward compatibility with different input names
  if (missing(nu) && !is.null(nu_r)) {
    nu <- vector(2, mode = "list" )
    nu[[1]] <- nu_r; nu[[2]] <- nu_f
    }
  if (missing(nu) && !is.null(tau_r)) {
    nu <- vector(2, mode = "list" )
    nu[[1]] <- tau_r; nu[[2]] <- tau_f
    }
  if ((missing(alpha) ||  is.logical(alpha)) && !is.null(kappa_r)) {
    alpha <- vector(2, mode = "list" )
    alpha[[1]] <- kappa_r; alpha[[2]] <- kappa_f
    }
  if ((missing(alpha) || is.logical(alpha)) && !is.null(alpha_r)) {
      alpha <- vector(2, mode = "list" )
      alpha[[1]] <- as.numeric(alpha_r); alpha[[2]] <- as.numeric(alpha_f)
    }
  if ((missing(psi) || is.logical(psi)) && !is.null(phi_r)) {
    psi <- vector(2, mode = "list" )
    psi[[1]] <- phi_r; psi[[2]] <- phi_f
    }
  if ((missing(psi) || is.logical(psi)) && !is.null(psi_r)) {
    psi <- vector(2, mode = "list" )
      psi[[1]] <- as.numeric(psi_r); psi[[2]] <- as.numeric(psi_f)
  }
  if (missing(lambda) && !is.null(lambda_r)) {
    lambda <- vector(2, mode = "list" )
    lambda[[1]] <- lambda_r; lambda[[2]] <- lambda_f
  }
  if (missing(Theta) && !is.null(Theta_r)) {
    Theta <- vector(2, mode = "list" )
    Theta[[1]] <- Theta_r; Theta[[2]] <- Theta_f
  }
  if (missing(pmix) && !is.null(pmix_ref)) {
    pmix <- c(pmix_ref, 1 - pmix_ref)    # assuming two groups
  }

  
  stopifnot("Number of groups as indicated in the lengths of parameters must
              match." = length(alpha) == lengths(list(psi, lambda, nu, Theta)))
  stopifnot("Number of dimensions must match." = 
              (all(lengths(alpha) == 1, lengths(psi) == 1, is.vector(lambda)) |
                 ((lengths(alpha) == dim(psi)[1]) & (dim(psi)[1] == dim(psi)[2]) &
                    lengths(alpha) == unlist(lapply(lambda, ncol)))))
  stopifnot("Provide the correct number of mixing proportions." = 
              length(pmix) == length(alpha))
  
  num_g <- length(alpha); n <- length(nu[[1]]); d <- length(alpha[[1]])

  if(is.null(pmix)) pmix <- as.matrix(c(rep(1 / num_g, num_g)), ncol = num_g)
  pmix <- as.vector(pmix)
  
  # If labels were not provided by the user or the number of labels provided or
  # the number of labels provided does not match num_g, define new labels
  if(is.null(labels) || (length(labels) != num_g)) {
    labels <- c("Reference", paste0("Focal_", 1:(num_g - 1)))
  }
 # print(labels)
  g <- c("r", paste0("f", 1:(num_g - 1)))
  names(alpha) <- paste("alpha", g, sep = "_")
  names(nu) <- paste("nu", g, sep = "_")
  names(lambda) <- paste("lambda", g, sep = "_")
  names(psi) <- paste("psi", g, sep = "_")
  names(Theta) <- paste("Theta", g, sep = "_")

  # Change any vector elements within the list Theta into diagonal matrices
  Theta <-  lapply(1:length(Theta), function(x) {
    if(is.vector(Theta[[x]])) {
      Theta[[x]]  <- diag(Theta[[x]]) 
    } else { 
      Theta[[x]] <- Theta[[x]]
      }
    })

  alpha <- lapply(alpha, as.matrix)
  psi <- lapply(psi, matrix, nrow = d, ncol = d)
  
  if (is.null(weights_item)) weights_item <- rep(1, n)
  if (is.null(weights_latent)) weights_latent <- rep(1, d)
  
   out <- compute_cai(weights_item, weights_latent, alpha, psi, lambda, nu, 
                      Theta, pmix, propsel, labels, cut_z, is_mi = FALSE)
   #colnames(out$summary) <- c(labels, paste0("E_R(", labels[2], ")"))
   
   if (out$propsel <= 0.01) warning("Proportion selected is 1% or less.")
   
   ai_ratio <-  as.data.frame(out$summary[5, (num_g + 1):(num_g + num_g - 1)] /
                             out$summary[5, 1])
  
   names(ai_ratio) <- paste0("Focal_", 1:(num_g - 1))
   row.names(ai_ratio) <- c("")
   out[["ai_ratio" ]] <- ai_ratio
   #print(ai_ratio)
   #print(typeof(ai_ratio))
  
   if (show_mi_result) { 
     pop_weights <- pmix
     lambda_average <- .weighted_average_list(lambda, weights = pop_weights)
     nu_average <- .weighted_average_list(nu, weights = pop_weights)
     Theta_average <- .weighted_average_list(Theta, weights = pop_weights)
     
     lambda_average_g <- nu_average_g <- Theta_average_g <- 
       vector(mode = "list", length = num_g)
     
     for (i in 1:num_g) {
       lambda_average_g[[i]] <- lambda_average
       nu_average_g[[i]] <- nu_average
       Theta_average_g[[i]] <- Theta_average
     }

     out_mi <- compute_cai(weights_item, weights_latent, 
                           alpha, psi, lambda_average_g, nu_average_g, Theta_average_g,
                           pmix, propsel, labels, cut_z, is_mi = TRUE)
     colnames(out_mi$summary) <- labels
     names(out_mi) <- paste0(names(out_mi), "_mi")
     
     out <- c(out, out_mi)
   }
   
    class(out) <- "PartInv"
    if (plot_contour) {
      plot.PartInv(out, labels = labels, which_result = "pi", ...)
      if(show_mi_result == TRUE) {
        plot.PartInv(out, labels = labels, which_result = "mi", ...)
      }
    }
   out
}

#' @rdname PartInvMulti_we
#' @export
PartInv <- PartInvMulti_we
