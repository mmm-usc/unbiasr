#' Effect size of item deletion on selection accuracy
#' 
#' \code{item_deletion_h} computes the effect size of the impact of item bias on  
#' selection accuracy indices under strict vs. partial invariance.
#' 
#' @param propsel: proportion of selection. If missing, computed using `cut_z`.
#' @param cut_z: pre-specified cutoff score on the observed composite. This 
#'        argument is ignored when `propsel` has input.
#' @param weights_item: a vector of item weights
#' @param n_dim: number of dimensions, 1 by default. If the user does not supply 
#'        a different value, proceeds with the assumption that the scale is 
#'        unidimensional.
#' @param n_i_per_dim: a vector containing the number of items in each 
#'        dimension; NULL by default. If the user provides a value for n_dim 
#'        that is > 1 but leaves n_i_per_dim = NULL, assumes that the subscales 
#'        have an equal number of items. 
#' @param weights_latent: a  vector of latent factor weights.
#' @param alpha_r: a vector of latent factor mean for the reference group.
#' @param alpha_f: (optional) a vector of latent factor mean for the focal  
#'        group;if no input, set equal to alpha_r.
#' @param psi_r: a matrix of latent factor variance for the reference group.
#' @param psi_f: (optional) a matrix of latent factor variance for the focal  
#'        group; if no input, set equal to psi_r.
#' @param lambda_r_p: a matrix of factor loadings for the reference group under
#'        the partial invariance condition.
#' @param lambda_f_p: (optional) a matrix of factor loadings for the focal group 
#'        under the partial invariance condition; if no input, set equal to 
#'        lambda_r.
#' @param nu_r_p: a matrix of measurement intercepts for the reference group 
#'        under the partial invariance condition.
#' @param nu_f_p: (optional) a matrix of measurement intercepts for the focal 
#'        group under the partial invariance condition; if no input, set equal 
#'        to nu_r.
#' @param theta_r_p: a matrix of the unique factor variances and covariances 
#'        for the reference group under the partial invariance condition.
#' @param theta_f_p: (optional) a matrix of the unique factor variances and 
#'        covariances for the focal group under the partial invariance
#'        condition; if no input, set equal to theta_r.
#' @param pmix_ref: Proportion of the reference group; 
#'        default to 0.5 (i.e., two populations have equal size)
#' @param plot_contour: logical; whether the contour of the two populations 
#'        should be plotted; default to TRUE.
#' @param return_all_outputs: logical; whether the outputs from each call 
#'        of \code{PartInv_Multi_we} should also be returned as part of the
#'        returned object; default to FALSE.
#' @return a list of the following two elements (or a list of the following 
#'        four elements if return_all_outputs == TRUE):
#'        - delta_table: a (8 x number of items) dataframe that stores Cohen's 
#'          h values for the effect size of the impact of item bias for each i
#'        - h_par_vs_str_ref: a list of length (number of items + 1) containing outputs 
#'          from \code{ref_acc_indices_h}. Each item in the list is a 
#'          restructured data frame comparing the various selection accuracy 
#'          indices for the reference group under the strict and partial 
#'          invariance conditions, and the corresponding h for each index. The
#'          first item in the list, 'full', contains the dataframe output for 
#'          when all items are included, and the remaining dataframes
#'          can be accessed by specifying i in '...$h_par_vs_str_ref$deleteitem_i'
#'  
#'          The following two lists of length (number of items + 1) are also 
#'          returned if the user indicates return_all_outputs == TRUE:
#'        
#'       - strict_results: contains outputs from \code{PartInvMulti_we} 
#'         under strict invariance.
#'       - partial_results: contains outputs from \code{PartInvMulti_we} 
#'         under partial invariance. 
#'         The first item in either list can be accessed through 
#'         '...$(partial/strict)_results$full' and contains the 
#'         \code{PartInvMulti_we} output when all items are included. Remaining 
#'         items in the list are of the 'deleteitem_i' form, and contain the 
#'         \code{PartInvMulti_we} output when a given item i is deleted
#'
#' @examples
#' # Multidimensional example
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' multi_dim <- item_deletion_h(propsel = .05, n_dim = 5,
#'                              weights_item = c(1/4, 1/4, 1/6, 1/6, 1/6),
#'                              weights_latent = c(0.5, 0.5),
#'                              alpha_r = c(0, 0),
#'                              alpha_f = c(-0.3, 0.1),
#'                              psi_r = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
#'                              lambda_r_p = lambda_matrix,
#'                              nu_r_p = c(.225, .025, .010, .240, .125),
#'                              nu_f_p = c(.225, -.05, .240, -.025, .125),
#'                              theta_r_p = diag(1, 5),
#'                              theta_f_p = c(1, .95, .80, .75, 1),
#'                              plot_contour = FALSE,
#'                              return_all_outputs = TRUE)
#' multi_dim$delta_table
#' multi_dim$h_par_vs_str_ref$full
#' multi_dim$h_par_vs_str_ref$deleteitem_1
#' multi_dim$strict_results$full
#' multi_dim$strict_results$deleteitem_1
#' multi_dim$partial_results$full
#' multi_dim$strict_results$deleteitem_1
#' 
#' # Single dimension examples
#' single_dim <- item_deletion_h(propsel = .10,
#'                               weights_item = c(1, 0.9, 0.8, 1),
#'                               weights_latent = 0.9,
#'                               alpha_r = 0.5,
#'                               alpha_f = 0,
#'                               psi_r = 1,
#'                               lambda_r_p = c(.3, .5, .9, .7),
#'                               nu_r_p = c(.225, .025, .010, .240),
#'                               nu_f_p = c(.225, -.05, .240, -.025),
#'                               theta_r_p = diag(.96, 4),
#'                               n_dim = 1, plot_contour = FALSE,
#'                               return_all_outputs = TRUE)
#' single_dim$delta_table
#' single_dim$h_par_vs_str_ref$full
#' single_dim$h_par_vs_str_ref$deleteitem_1
#' single_dim$strict_results$full
#' single_dim$strict_results$deleteitem_1
#' single_dim$partial_results$full
#' single_dim$strict_results$deleteitem_1
#' 
#' # If we specify return_all_outputs = FALSE
#' single_dim2 <- item_deletion_h(propsel = .10,
#'                                weights_item = c(1, 0.9, 0.8, 1),
#'                                weights_latent = 0.9,
#'                                alpha_r = 0.5,
#'                                alpha_f = 0,
#'                                psi_r = 1,
#'                                lambda_r_p = c(.3, .5, .9, .7),
#'                                nu_r_p = c(.225, .025, .010, .240),
#'                                nu_f_p = c(.225, -.05, .240, -.025),
#'                                theta_r_p = diag(.96, 4),
#'                                n_dim = 1, plot_contour = FALSE,
#'                                return_all_outputs = FALSE)
#' single_dim2$delta_table
#' single_dim2$h_par_vs_str_ref$full
#' single_dim2$h_par_vs_str_ref$deleteitem_1                                      

item_deletion_h <- function(propsel, cut_z = NULL, 
                            weights_item, 
                            n_dim = 1,
                            n_i_per_dim = NULL, 
                            weights_latent,
                            alpha_r, alpha_f = alpha_r,
                            psi_r, psi_f = psi_r,
                            lambda_r_p, lambda_f_p = lambda_r_p, 
                            nu_r_p, nu_f_p = nu_r_p,
                            theta_r_p, theta_f_p = theta_r_p,
                            pmix_ref = 0.5, 
                            plot_contour = TRUE,
                            return_all_outputs = FALSE,
                            ...) {
  
  # Start with pre-allocating space:
  n_items <- length(weights_item)
  store_str <- store_par <- h_par_vs_str_ref <- vector(mode = "list",
                                                       n_items + 1)
  delta_table <- as.data.frame(matrix(nrow = 8, ncol = n_items))
  h_R_Ef_full <- c()
  h_R_Ef_del <- delta_h_R_Ef <- as.data.frame(matrix(nrow = 8, ncol = n_items))
  performance <- perf_del1 <- as.data.frame(matrix(nrow = 3, ncol = n_items)) 
  
 # Call PartInvMulti_we with the full item set under strict invariance
  store_str[[1]] <- PartInvMulti_we(propsel, cut_z = cut_z, 
                                    weights_item, weights_latent,
                                    alpha_r = alpha_r,
                                    alpha_f = alpha_f,
                                    psi_r = psi_r,
                                    psi_f = psi_f,
                                    lambda_r = lambda_f_p * (1 - pmix_ref) + 
                                               lambda_r_p * pmix_ref,
                                    nu_r = nu_f_p * (1 - pmix_ref) + 
                                           nu_r_p * pmix_ref,
                                    Theta_r = theta_f_p * (1 - pmix_ref) + 
                                              theta_r_p * pmix_ref,
                                    pmix_ref = pmix_ref, 
                                    plot_contour = plot_contour)

  # Call PartInvMulti_we with the full item set under partial invariance
  store_par[[1]] <- PartInvMulti_we(propsel, cut_z = cut_z, 
                                    weights_item, weights_latent,
                                    alpha_r = alpha_r,
                                    alpha_f = alpha_f,
                                    psi_r = psi_r,
                                    psi_f = psi_f,
                                    lambda_r = lambda_r_p,
                                    lambda_f = lambda_f_p,
                                    nu_r = nu_r_p,
                                    nu_f = nu_f_p,
                                    Theta_r = theta_r_p,
                                    Theta_f = theta_f_p,
                                    pmix_ref = pmix_ref, 
                                    plot_contour = plot_contour) 
  
  # Compare accuracy indices for reference group under strict vs. partial
  # invariance conditions and compute h for full item set
  h_par_vs_str_ref[[1]] <- ref_acc_indices_h(store_str[[1]], store_par[[1]]) 
  h_R_Ef_full <- cohens_h(store_par[[1]]$summary$Reference, 
                                     store_par[[1]]$summary$E_R.Focal.)
  perf_full <- get_perf(pmix_ref, store_par[[1]]$summary)
  
  # (Re)set the proportion selected based on the PartInvMulti_we output when all
  # items are included in the strict invariance condition
  propsel <- store_str[[1]]$propsel
  cut_z <- NULL
  
  # Delete one item at a time by assigning 0 to index i and redistributing
  # weights for the subscale; populate store_str and store_par
  for (i in seq_len(length(weights_item) + 1)[-1]) {
    
    # Assign a weight of 0 to the item to be deleted (indexed at i - 1), and 
    # redistribute the weight from this item across the non-deleted items
    take_one_out <- redistribute_weights(weights_item, n_dim = n_dim,
                                       n_i_per_dim = n_i_per_dim, del_i = i - 1)
    
    # Call PartInvMulti_we with the new weights under strict invariance
    store_str[[i]] <- PartInvMulti_we(propsel, cut_z = cut_z, 
                                      take_one_out, weights_latent,
                                      alpha_r = alpha_r,
                                      alpha_f = alpha_f,
                                      psi_r = psi_r,
                                      psi_f = psi_f,
                                      lambda_r = lambda_f_p * (1 - pmix_ref) + 
                                                 lambda_r_p * pmix_ref,
                                      nu_r = nu_f_p * (1 - pmix_ref) +
                                             nu_r_p * pmix_ref,
                                      Theta_r = theta_f_p * (1 - pmix_ref) + 
                                                theta_r_p * pmix_ref,
                                      pmix_ref = pmix_ref, 
                                      plot_contour = plot_contour)
    # Call PartInvMulti_we with the new weights under partial invariance
    store_par[[i]] <- PartInvMulti_we(propsel, cut_z = cut_z,
                                      take_one_out, weights_latent,
                                      alpha_r = alpha_r,
                                      alpha_f = alpha_f,
                                      psi_r = psi_r,
                                      psi_f = psi_f,
                                      lambda_r = lambda_r_p,
                                      nu_r = nu_r_p,
                                      nu_f = nu_f_p,
                                      Theta_r = theta_r_p,
                                      Theta_f = theta_f_p,
                                      pmix_ref = pmix_ref, 
                                      plot_contour = plot_contour)
                    
    # Compute h for the difference in accuracy indices for reference group under
    # strict vs. partial invariance conditions 
    h_par_vs_str_ref[[i]] <- ref_acc_indices_h(store_str[[i]], store_par[[i]]) 
    # Compute the change in Cohen's h comparing accuracy indices for the 
    # reference group under strict vs. partial invariance when item i is deleted
    # i.e. the change in h_par_vs_str_ref
    delta_table[i - 1] <- delta_h(h_par_vs_str_ref[[1]]$h, 
                                  h_par_vs_str_ref[[i]]$h)
    
    # Compute h for the difference in accuracy indices under partial invariance
    # for the reference group vs. for the expected accuracy indices for the 
    # focal group if it followed the same distribution as the reference group 
    # (E_R.Focal.)
    h_R_Ef_del[i - 1] <- round(cohens_h(store_par[[i]]$summary$Reference, 
                                        store_par[[i]]$summary$E_R.Focal.), 3)
    # Compute the change in Cohen's h comparing accuracy indices under partial
    # invariance for the reference group vs. for the expected accuracy indices
    # for the focal group if it followed the same distribution as the reference
    # group when item i is deleted, i.e. the change in h_R_Ef_del
    delta_h_R_Ef[i - 1] <- round(delta_h(h_R_Ef_full, h_R_Ef_del[i - 1]), 3)
    
    # Compute overall SR, SE, SP indices under partial invariance by weighting 
    # accuracy indices for the reference and focal groups by their group
    # proportions
    perf_del1[i - 1] <- get_perf(pmix_ref, store_par[[i]]$summary)
    # Compute how the overall SR, SE, SP indices change when an item is deleted
    performance[i - 1] <- cohens_h(perf_full, perf_del1[i - 1])
  }
  
  # Format stored variables
  h_R_Ef <- as.data.frame(cbind(round(h_R_Ef_full, 3), h_R_Ef_del))
  weighted_SR_SE_SP <- as.data.frame(cbind(round(perf_full, 3),
                                          round(perf_del1, 3)))
  performance <- as.data.frame(round(performance, 3))
  colnames <- paste0(c("full", rep("deleteitem_", n_items)), c("", 1:n_items))
  names(store_str) <- names(store_par) <- names(h_par_vs_str_ref) <- names(h_R_Ef) <-  
    names(weighted_SR_SE_SP) <- colnames
  names(delta_table) <- names(delta_h_R_Ef) <- 
    paste0(rep(paste0('\u0394', "h(-")), c(1:n_items), c(rep(")")))
  names(performance) <- paste0(rep(paste0("h(-")), c(1:n_items), c(rep(")")))
  rownames(delta_table) <- rownames(h_R_Ef) <- rownames(delta_h_R_Ef) <-
    c("TP","FP","TN","FN", "PS", "SR", "SE", "SP")
  rownames(performance) <- rownames(weighted_SR_SE_SP) <- c("SR", "SE", "SP")
  
  if (return_all_outputs == TRUE) {
    return(list("performance" = performance,
                "delta_h_R_Ef" = delta_h_R_Ef,
                "delta_table" = delta_table, 
                "h_par_vs_str_ref" = h_par_vs_str_ref,
                "weighted_SR_SE_SP" = weighted_SR_SE_SP,
                "h_R_Ef" = h_R_Ef,
                "strict_results" = store_str, 
                "partial_results" = store_par))
  } else {
    return(list("performance" = performance,
                "delta_h_R_Ef" = delta_h_R_Ef,
                "delta_table" = delta_table, 
                "h_par_vs_str_ref" = h_par_vs_str_ref))
  }
}