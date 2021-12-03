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
#' @return a list of the following elements:
#'        - h_overall_SR_SE_SP_par: a (3 x number of items) data frame that 
#'          stores Cohen's h values comparing overall SR, SE, SP  under partial
#'          invariance when item i is deleted (after the accuracy indices were 
#'          weighted by the focal and reference group proportions). 
#'        - delta_h_str_vs_par_ref: a (8 x number of items) data frame that 
#'          stores the change in Cohen's h comparing accuracy indices for the 
#'          reference group under strict vs. partial invariance when item i is 
#'          deleted i.e. the change in h_str_vs_par_ref.
#'        - h_str_vs_par_ref: a list of length (number of items + 1). Each item 
#'          in the list is a data frame containing Cohen's h comparing the 
#'          selection accuracy indices for the reference group under the strict
#'          vs. partial invariance conditions, for a given set of items. The
#'          first element in the list, 'full', corresponds to the comparison
#'          when all items are included. The remaining elements correspond to
#'          the comparison when item i is deleted, and these can be accessed by
#'          specifying i in '...$h_str_vs_par_ref$deleteitem_i'
#'  
#'         The following lists are also returned if return_all_outputs == TRUE)
#'          The following two lists of length (number of items + 1) are also 
#'          returned if the user indicates return_all_outputs == TRUE:
#'        
#'       - overall_SR_SE_SP_par: a (3 x number of items + 1) data frame 
#'         containing overall SR, SE, SP (after the 
#'         accuracy indices were weighted by the focal and reference group 
#'         proportions) under partial invariance. The first column refers to the
#'         case with the full item set, remaining columns refer to the scenario
#'         when item i is deleted.
#'       - strict_results: a list of length (number of items + 1) containing
#'         outputs from \code{PartInvMulti_we} under strict invariance.
#'       - partial_results: a list of length (number of items + 1) containing
#'         outputs from \code{PartInvMulti_we} under partial invariance. 
#'         The first item in either list can be accessed through 
#'         '...$(partial/strict)_results$full' and contains the 
#'         \code{PartInvMulti_we} output when all items are included. Remaining 
#'         items in the list are of the 'deleteitem_i' form, and contain the 
#'         \code{PartInvMulti_we} output when item i is deleted.
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
#' multi_dim$delta_h_str_vs_par_ref
#' multi_dim$h_str_vs_par_ref$full
#' multi_dim$h_str_vs_par_ref$deleteitem_1
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
#' single_dim$delta_h_str_vs_par_ref
#' single_dim$h_str_vs_par_ref$full
#' single_dim$h_str_vs_par_ref$deleteitem_1
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
#' single_dim2$delta_h_str_vs_par_ref
#' single_dim2$h_str_vs_par_ref$full
#' single_dim2$h_str_vs_par_ref$deleteitem_1                                      

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
  store_str <- store_par <- h_str_vs_par_ref_list <- vector(mode = "list",
                                                       n_items + 1)
  delta_h_str_vs_par_ref <- as.data.frame(matrix(nrow = 8, ncol = n_items))
  h_R_Ef_full <- h_str_vs_par_ref_full <- c()
  h_R_Ef_del <- h_str_vs_par_ref_del <- delta_h_R_vs_Ef_par <- 
    as.data.frame(matrix(nrow = 8, ncol = n_items))
  h_overall_SR_SE_SP_par <- overall3_par_del1 <- 
    as.data.frame(matrix(nrow = 3, ncol = n_items)) 
  
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
  h_str_vs_par_ref_list[[1]] <- ref_acc_indices_h(store_str[[1]], 
                                                  store_par[[1]]) 
  h_str_vs_par_ref_full <- h_str_vs_par_ref_list[[1]]$h
  h_R_Ef_full <- cohens_h(store_par[[1]]$summary$Reference, 
                                     store_par[[1]]$summary$E_R.Focal.)
  # Re-weight the accuracy indices by focal and group proportions to compute 
  # overall accuracy indices under partial invariance for the full item set
  overall3_par_full <- get_perf(pmix_ref, store_par[[1]]$summary)
  
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
    h_str_vs_par_ref_list[[i]] <- ref_acc_indices_h(store_str[[i]], 
                                                    store_par[[i]])
    h_str_vs_par_ref_del[i - 1] <- h_str_vs_par_ref_list[[i]]$h
    # Compute the change in Cohen's h comparing accuracy indices for the 
    # reference group under strict vs. partial invariance when item i is deleted
    # i.e. the change in h_str_vs_par_ref 
    delta_h_str_vs_par_ref[i - 1] <- delta_h(h_str_vs_par_ref_list[[1]]$h, 
                                  h_str_vs_par_ref_list[[i]]$h)
    
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
    delta_h_R_vs_Ef_par[i - 1] <- round(delta_h(h_R_Ef_full,
                                                h_R_Ef_del[i - 1]), 3)
    
    # Compute overall SR, SE, SP indices under partial invariance by weighting 
    # accuracy indices for the reference and focal groups by their group
    # proportions
    overall3_par_del1[i - 1] <- get_perf(pmix_ref, store_par[[i]]$summary)
    # Compute how the overall SR, SE, SP indices change when an item is deleted
    h_overall_SR_SE_SP_par[i - 1] <- cohens_h(overall3_par_full, 
                                              overall3_par_del1[i - 1])
  }
  
  # Format stored variables
  h_R_Ef <- as.data.frame(cbind(round(h_R_Ef_full, 3), h_R_Ef_del))
  h_str_vs_par_ref <- as.data.frame(cbind(round(h_str_vs_par_ref_full, 3), 
                                          h_str_vs_par_ref_del))
  overall_SR_SE_SP_par <- as.data.frame(cbind(round(overall3_par_full, 3),
                                          round(overall3_par_del1, 3)))
  h_overall_SR_SE_SP_par <- as.data.frame(round(h_overall_SR_SE_SP_par, 3))
  
  #colnames <- paste0(c("h(full)", rep("deleteitem_", n_items)), c("", 1:n_items))
  colnames <- c("h(full)", paste0(rep(paste0("h(-")), c(1:n_items), c(rep(")"))))
  names(store_str) <- names(store_par) <- names(h_str_vs_par_ref_list) <-
    names(h_R_Ef) <- names(h_str_vs_par_ref) <- names(overall_SR_SE_SP_par) <-
    colnames
  names(delta_h_str_vs_par_ref) <- names(delta_h_R_vs_Ef_par) <- 
    paste0(rep(paste0('\u0394', "h(-")), c(1:n_items), c(rep(")")))
  names(h_overall_SR_SE_SP_par) <- 
    paste0(rep(paste0("h(-")), c(1:n_items), c(rep(")")))
  rownames(delta_h_str_vs_par_ref) <- rownames(h_R_Ef) <- 
    rownames(delta_h_R_vs_Ef_par) <- rownames(h_str_vs_par_ref) <- 
    c("TP","FP","TN","FN", "PS", "SR", "SE", "SP")
  rownames(h_overall_SR_SE_SP_par) <- rownames(overall_SR_SE_SP_par) <- 
    c("SR", "SE", "SP")
  
  if (return_all_outputs == TRUE) {
    return(list("h_overall_SR_SE_SP_par" = h_overall_SR_SE_SP_par,
                "delta_h_R_vs_Ef_par" = delta_h_R_vs_Ef_par,
                "delta_h_str_vs_par_ref" = delta_h_str_vs_par_ref, 
                "h_str_vs_par_ref" = h_str_vs_par_ref,
                "overall_SR_SE_SP_par" = overall_SR_SE_SP_par,
                "h_R_vs_Ef_par" = h_R_Ef,
                "h_str_vs_par_ref_list" = h_str_vs_par_ref_list,
                "strict_results" = store_str, 
                "partial_results" = store_par))
  } else {
    return(list("h_overall_SR_SE_SP_par" = h_overall_SR_SE_SP_par,
                "delta_h_R_vs_Ef_par" = delta_h_R_vs_Ef_par,
                "delta_h_str_vs_par_ref" = delta_h_str_vs_par_ref, 
                "h_str_vs_par_ref" = h_str_vs_par_ref))
  }
}