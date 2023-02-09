#' @importFrom stats qchisq pnorm qnorm nlminb
#' @importFrom mnormt pmnorm
NULL

#' Evaluating selection accuracy for two or more groups based on the MCAA 
#' Framework
#' 
#' \code{PartInv, PartInvMulti_we} evaluates partial measurement invariance 
#' using the multidimensional classification accuracy analysis 
#' (Lai & Zhang, 2022), which is an extension of Millsap & Kwok's (2004) 
#' approach.
#'
#' @param propsel Proportion of selection. If missing, computed using `cut_z`.
#' @param cut_z Pre-specified cutoff score on the observed composite. This
#'     argument is ignored when `propsel` has input.
#' @param weights_item A vector of item weights.
#' @param weights_latent A vector of latent factor weights.
#' @param alpha A list of length `g` containing `1 x d` latent factor mean 
#'     vectors where `g` is the number of groups and `d` is the number of latent 
#'     dimensions. The first element is assumed to belong to the reference group.
#' @param psi A list of length `g` containing `d x d` latent factor 
#'     variance-covariance matrices where `g` is the number of groups and `d` is 
#'     the number of latent dimensions. The first element is assumed to belong 
#'     to the reference group.
#' @param lambda A list of length `g` containing `n x d` factor loading matrices 
#'     where `g` is the number of groups, `d` is the number of latent dimensions, 
#'     and `n` is the number of items in the scale. The first element is assumed 
#'     to belong to the reference group.
#' @param nu A list of length `g` containing `1 x n` measurement intercept
#'     vectors where `g` is the number of groups and `n` is the number of items 
#'     in the scale. The first element is assumed to belong to the reference 
#'     group.
#' @param Theta A list of length `g` containing `1 x n` vectors or `n x n` 
#'     matrices of unique factor variances and covariances, where `g` is the 
#'     number of groups and `n` is the number of items in the scale. The first 
#'     element is assumed to belong to the reference group.
#' @param pmix List of length `g` containing the mixing proportions of each 
#'     group. If `NULL`, defaults to `1/g` for each group (i.e., the populations 
#'     have equal size).
#' @param plot_contour Logical; whether the contour of the populations should be
#'     plotted; `TRUE` by default.
#' @param show_mi_result If \code{TRUE}, perform selection accuracy analysis
#'     for both the input parameters and the implied parameters based on a
#'     strict invariance model, with common parameter values as weighted
#'     averages of the input values using `pmix`.
#' @param labels A character vector with `g` elements to label the reference
#'     and focal groups on the plot, where `g` is the number of groups. If not
#'     provided, groups are labeled automatically as 'Reference' (for the first
#'     group) and 'Focal_1' through 'Focal_(g-1)', where `g` is the number of 
#'     groups.
#' @param ... Other arguments passed to the \code{\link[graphics]{contour}}
#'     function.
#' @param alpha_r,alpha_f,nu_r,nu_f,Theta_r,Theta_f,psi_r,psi_f,lambda_r,lambda_f,phi_r,phi_f,tau_r,tau_f,kappa_r,kappa_f,pmix_ref 
#'     Deprecated; included only for backward compatibility. When comparing two 
#'     groups, parameters with the '_r' suffix refer to the reference group while
#'     parameters with the '_f' suffix refer to the focal group.
#' @return The output will be a list of six elements and a plot if
#'     \code{plot_contour == TRUE}:
#'         \item{propsel}{Proportion selected.}
#'         \item{cutpt_xi}{Cut point on the latent scale (xi).}
#'         \item{cutpt_z}{Cut point on the observed scale (Z).}
#'         \item{summary}{A `8 x (g + g - 1)` table, with columns representing 
#'             the reference and `g - 1` focal groups, and the expected results 
#'             if the latent distribution of `g - 1` focal group match the 
#'             reference group. The rows represent probabilities of true 
#'             positive (A), false positive (B), true negative (C), false 
#'             negative (D); proportion selected, success ratio, sensitivity, 
#'             and specificity.}
#'         \item{bivardata}{List of length `5` containing `1 x g` vectors of  
#'             latent and observed means, standard deviations, and covariances 
#'             computed for each groups.}
#'         \item{ai_ratio}{A list of length `g - 1` containing the Adverse 
#'             Impact (AI) ratio computed for each focal group. A result less
#'             than 80% may be considered evidence of adverse impact.}
#'      If \code{show_mi_result = TRUE}, the returned list will have the 
#'      additional elements below: 
#'          \item{propsel_mi}{Proportion selected under strict invariance.}
#'          \item{cutpt_xi_mi}{Cut point on the latent scale (xi) under strict
#'             invariance.}
#'          \item{cutpt_z_mi}{Cut point on the observed scale (Z) under strict 
#'             invariance.}
#'         \item{summary_mi}{A `8 x (g + g - 1)` table, with columns 
#'             representing the reference and `g - 1` focal groups and the 
#'             expected results if the latent distributions of `g - 1` focal 
#'             groups match the reference group, under strict invariance. The 
#'             rows represent probabilities of true positive (A), false positive 
#'             (B), true negative (C), false negative (D); proportion selected,
#'             success ratio, sensitivity, and specificity.}
#'          \item{bivardata_mi}{List of length `5` containing `1 x g` vectors of 
#'             latent and observed means, standard deviations, and covariances 
#'             computed for each group under strict invariance.}
#' @examples
#' # Two groups, single dimension
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
#'         show_mi_result = TRUE)
#' # Two groups, multiple dimensions
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
#' PartInv(propsel = .10,
#'         weights_item = c(1/3, 1/3, 1/3, 1/3),
#'         weights_latent = 1,
#'         alpha_r = 0.5,
#'         alpha_f = 0,
#'         psi_r = 1,
#'         lambda_r = c(.3, .5, .9, .7),
#'         nu_r = c(.225, .025, .010, .240),
#'         nu_f = c(.225, -.05, .240, -.025),
#'         Theta_r = diag(.96, 4),
#'         labels = c("female", "male"),
#'         show_mi_result = TRUE)
#' # Multiple groups, multiple dimensions
#' lambda_matrix <- lambda_matrix1 <- lambda_matrix2 <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <-  c(.322, .655); lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' lambda_matrix1[1:2, 1] <-  c(.392, .665); lambda_matrix1[3:5, 2] <- c(.388, .725, .523)
#' lambda_matrix2[1:2, 1] <-  c(.372, .650); lambda_matrix2[3:5, 2] <- c(.368, .7, .543)
#' # 2 dimensions, 5 items (2,3), 3 groups
#' PartInv(propsel = 0.25, cut_z = 2, pmix = c(1/3, 1/3, 1/3),
#'        alpha = list(c(1,1), c(0,0), c(.5, .5)),
#'        psi = list(c(1, 0.2, 0.2, 1), c(1, 0.3, 0.3, 1), c(1, 0.4, 0.4, 1)),
#'        nu = list(c(rep(1,5)), c(rep(1.5,5)),c(rep(1.2,5))),
#'        lambda = list(lambda_matrix, lambda_matrix1, lambda_matrix2),
#'        Theta = list(c(rep(.1,5)), c(rep(.4,5)), c(rep(.3,5))),
#'        plot_contour = TRUE, labels = c("Group 1", "Group 2", "Group 3"),
#'        custom_colors = c("salmon1", "lightgreen", "skyblue1"), 
#'        show_mi_result = TRUE)
#' @export
PartInvMulti_we <- function(propsel = NULL, cut_z = NULL,
                            weights_item = NULL,
                            weights_latent = NULL,
                            alpha, psi, lambda, Theta, nu,
                            pmix = 0.5,
                            pmix_ref = 0.5, plot_contour = FALSE,
                            show_mi_result = FALSE,
                            labels = NULL,
                            kappa_r = NULL, kappa_f = kappa_r,
                            alpha_r = NULL, alpha_f = alpha_r,
                            phi_r = NULL, phi_f = phi_r,
                            psi_r = NULL, psi_f = psi_r,
                            lambda_r = NULL, lambda_f = lambda_r,
                            tau_r = NULL, tau_f = tau_r,
                            nu_r = NULL, nu_f = nu_r,
                            Theta_r = NULL, Theta_f = Theta_r,
                            ...) {
  
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
   
   if (out$propsel <= 0.01) warning("Proportion selected is 1% or less.")
   
   ai_ratio <-  as.data.frame(out$summary[5, (num_g + 1):(num_g + num_g - 1)] /
                             out$summary[5, 1])
  
   names(ai_ratio) <- labels[-1]
   row.names(ai_ratio) <- c("")
   out[["ai_ratio"]] <- ai_ratio
  
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

     out_mi <- compute_cai(weights_item, weights_latent, alpha, psi, 
                           lambda_average_g, nu_average_g, Theta_average_g,
                           pmix, propsel, labels, cut_z, is_mi = TRUE)
     colnames(out_mi$summary) <- labels
     names(out_mi) <- paste0(names(out_mi), "_mi")
     out <- c(out, out_mi)
   }
   
  
    if (plot_contour) {
      plot.PartInv(out, labels = labels, which_result = "pi", ...)
      if(show_mi_result == TRUE) {
        plot.PartInv(out, labels = labels, which_result = "mi", ...)
      }
    }
   class(out) <- "PartInv"
   out
}

#' @rdname PartInvMulti_we
#' @export
PartInv <- PartInvMulti_we
