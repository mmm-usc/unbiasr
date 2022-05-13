#' @title
#' Effect size of item deletion on selection accuracy
#' 
#' @name
#' item_deletion_h
#' 
#' @description
#' \code{item_deletion_h} computes Cohen's h effect size for the impact of
#' deleting an item on selection accuracy indices (TP, FP, TN, FN, PS, SR, SE, 
#' SP) under a number of combinations of conditions such as partial vs. strict 
#' invariance, for the reference group vs. the focal group if it followed the 
#' same distribution as the reference group under partial invariance, and for 
#' the weighted average of reference and focal groups under partial invariance.
#' 
#' @param propsel Proportion of selection. If missing, computed using `cut_z`.
#' @param cut_z Pre-specified cutoff score on the observed composite. This 
#'        argument is ignored when `propsel` has input.
#' @param weights_item A vector of item weights.
#' @param weights_latent A  vector of latent factor weights.
#' @param alpha_r A vector of latent factor mean for the reference group.
#' @param alpha_f (optional) A vector of latent factor mean for the focal  
#'        group; if no input, set equal to `alpha_r`.
#' @param psi_r a matrix of latent factor variance for the reference group.
#' @param psi_f (optional) A matrix of latent factor variance for the focal  
#'        group; if no input, set equal to `psi_r`.
#' @param lambda_r A matrix of factor loadings for the reference group under
#'        the partial invariance condition.
#' @param lambda_f (optional) A matrix of factor loadings for the focal group 
#'        under the partial invariance condition; if no input, set equal to 
#'        `lambda_r`.
#' @param nu_r A matrix of measurement intercepts for the reference group 
#'        under the partial invariance condition.
#' @param nu_f (optional) A matrix of measurement intercepts for the focal 
#'        group under the partial invariance condition; if no input, set equal 
#'        to `nu_r`.
#' @param Theta_r A matrix of the unique factor variances and covariances 
#'        for the reference group under the partial invariance condition.
#' @param Theta_f (optional) A matrix of the unique factor variances and 
#'        covariances for the focal group under the partial invariance
#'        condition; if no input, set equal to `Theta_r`.
#' @param pmix_ref Proportion of the reference group; default to 0.5 (i.e., two 
#'        populations have equal size)
#' @param plot_contour Logical; whether the contour of the two populations 
#'        should be plotted; default to `TRUE`.
#' @param n_dim Number of dimensions, 1 by default. If the user does not supply 
#'        a different value, proceeds with the assumption that the scale is 
#'        unidimensional.
#' @param n_i_per_dim A vector containing the number of items in each 
#'        dimension; `NULL` by default. If the user provides a value for `n_dim` 
#'        that is \eqn{> 1} but leaves \code{n_i_per_dim = NULL}, assumes that 
#'        the subscales have an equal number of items. 
#' @param return_detailed Logical; whether additional data frames should be r
#'        returned object; default to `FALSE`. If 
#'        \code{return_detailed == FALSE}, returns a list of length 2 with the 
#'        elements: `h_aggregate_cai.par` (a data frame) and`delta_h` (a list). 
#'        If set to `TRUE`, returns `h` and `raw` as an additional elements.
#' @return A list with 3 elements if \code{return_detailed == FALSE}, 4 elements
#'         if \code{return_detailed == TRUE}.
#'        \item{h_aggregate_cai.par}{A (3 x number of items) data frame that 
#'          stores Cohen's h values for the comparison between aggregate SR, SE, 
#'          SP under partial invariance for the full item set vs. aggregate SR, 
#'          SE, SP under partial invariance when item i is deleted. 'Overall'
#'          refers to the weighting of the accuracy indices for focal and 
#'          reference group proportions.}
#'        \item{delta_h}{A list of length 2 with elements `h_R_vs_Ef.par` and 
#'        `h_str_vs_par`, which stores `ref` and `foc`. }
#'        \item{delta_h$h_R_vs_Ef.par}{A (8 x number of items) data frame that 
#'          stores the Cohen's h effect size for the change in `h$R_vs_Ef_par` 
#'          when the full item set is included vs. `h$R_vs_Ef_par` when item i 
#'          is deleted.} 
#'        \item{delta_h$h_str_vs_par$ref}{A (8 x number of items) data frame that 
#'          stores the Cohen's h effect size for the change in `h$str_vs_par$ref`
#'          when the full item set is included vs. `h$str_vs_par$ref` when item i 
#'          is deleted, i.e., the impact of deleting an item on the h values 
#'          comparing performance under strict invariance vs. under partial
#'          invariance, for the reference group.}
#'        \item{delta_h$h_str_vs_par$foc}{A (8 x number of items) data frame that 
#'          stores the Cohen's h effect size for the change in `h$str_vs_par$foc`
#'          when the full item set is included vs. `raw$h_str_vs_par_list$foc` 
#'          when item i is deleted.}
#'        \item{h}{A list of length 2 storing `R_vs_ef.par` and `str_vs_par`, 
#'          which stores `ref` and `foc`.}
#'        \item{h$R_vs_Ef.par}{A (8 x (number of items + 1)) data frame that 
#'          stores Cohen's h values for the comparison between accuracy indices 
#'          for the reference group vs. the expected accuracy indices for the 
#'          focal group if it followed the same distribution as the reference 
#'          group, under partial invariance, for a given set of items.}
#'        \item{h$str_vs_par$ref}{A (8 x (number of items + 1)) data frame that 
#'          stores Cohen's h values for the comparison between accuracy indices 
#'          for the reference group under strict vs. partial invariance, for a
#'          given set of items.}
#'        \item{h$str_vs_par$foc}{A (8 x (number of items + 1)) data frame that 
#'          stores Cohen's h values for the comparison between accuracy indices 
#'          for the focal group under strict vs. partial invariance, for a
#'          given set of items.}
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
#'                              lambda_r = lambda_matrix,
#'                              nu_r = c(.225, .025, .010, .240, .125),
#'                              nu_f = c(.225, -.05, .240, -.025, .125),
#'                              Theta_r = diag(1, 5),
#'                              Theta_f = c(1, .95, .80, .75, 1),
#'                              plot_contour = TRUE,
#'                              return_detailed = TRUE)
#' # Single dimension example
#' single_dim <- item_deletion_h(propsel = .10,
#'                                weights_item = c(1, 0.9, 0.8, 1),
#'                                weights_latent = 0.9,
#'                                alpha_r = 0.5,
#'                                alpha_f = 0,
#'                                psi_r = 1,
#'                                lambda_r = c(.3, .5, .9, .7),
#'                                nu_r = c(.225, .025, .010, .240),
#'                                nu_f = c(.225, -.05, .240, -.025),
#'                                Theta_r = diag(.96, 4),
#'                                n_dim = 1, plot_contour = TRUE,
#'                                return_detailed = TRUE)
#' @export
item_deletion_h <- function(propsel, 
                            cut_z = NULL, 
                            weights_item, 
                            weights_latent,
                            alpha_r, 
                            alpha_f = alpha_r,
                            psi_r, 
                            psi_f = psi_r,
                            lambda_r, 
                            lambda_f = lambda_r, 
                            nu_r, 
                            nu_f = nu_r,
                            Theta_r, 
                            Theta_f = Theta_r,
                            pmix_ref = 0.5, 
                            plot_contour = TRUE,
                            show_mi_result = TRUE,
                            labels = c("Reference", "Focal"),
                            n_dim = 1,
                            n_i_per_dim = NULL, 
                            return_detailed = FALSE,
                            biased_items_only = TRUE,
                            user_specified_items = NULL,
                            ...) {
  N <- length(weights_item)
  
  # Determine which set of items will be returned
  return_items <- c()
  if(is.null(user_specified_items)) {
    if(biased_items_only == FALSE){ 
      return_items <- seq_len(N)
    } else { # default is to return only the biased items.
      return_items <- determine_biased_items(lambda_r = lambda_r,
                                             lambda_f = lambda_f, 
                                             nu_r = nu_r, nu_f = nu_f, 
                                             Theta_r = Theta_r,
                                             Theta_f = Theta_f)}
  } else {
    if (!all(user_specified_items == floor(user_specified_items))) { 
      stop("'user_specified_items' should only contain integers corresponding 
             to item indices.")}
    if (!all(user_specified_items < N)) { 
      stop("'user_specified_items' cannot take integers larger than the scale
             length.")}
    return_items <- user_specified_items
  }
  # print(return_items)
  
  # Start with pre-allocating space:
  store_str <- store_par <- str_par_ref_list <- str_par_foc_list <- 
    vector(mode = "list", N + 1)
  
  delta_h_str_vs_par_ref <- delta_h_str_vs_par_foc <- delta_h_R_vs_Ef <- 
    as.data.frame(matrix(nrow = 8, ncol = N))
  
  h_R_Ef <- h_str_vs_par_ref <- h_str_vs_par_foc <- 
    as.data.frame(matrix(nrow = 8, ncol = N + 1))
  
  aggregate_par <- aggregate_str <- h_aggregate_str_par <-
    as.data.frame(matrix(nrow = 4, ncol = N + 1))
  
  h_aggregate_par <-  delta_h_str_par_aggregate <- 
    as.data.frame(matrix(nrow = 4, ncol = N)) 
  
  AI_ratios <- as.data.frame(matrix(nrow = 2, ncol = N + 1))
  # Call PartInv with the full item set under strict invariance
  store_str[[1]] <- PartInv(propsel, 
                            cut_z = cut_z, 
                            weights_item, weights_latent,
                            alpha_r = alpha_r,
                            alpha_f = alpha_f,
                            psi_r = psi_r,
                            psi_f = psi_f,
                            lambda_r = lambda_f * (1 - pmix_ref) +
                              lambda_r * pmix_ref,
                            nu_r = nu_f * (1 - pmix_ref) + nu_r * pmix_ref,
                            Theta_r = Theta_f * (1 - pmix_ref) + 
                              Theta_r * pmix_ref,
                            pmix_ref = pmix_ref, 
                            plot_contour = plot_contour, 
                            labels = c("Reference", "Focal"),
                            show_mi_result=show_mi_result)
  
  class(store_str[[1]]) <- "PartInv"
  
  # Call PartInv with the full item set under partial invariance
  store_par[[1]] <- PartInv(propsel, 
                            cut_z = cut_z, 
                            weights_item, 
                            weights_latent,
                            alpha_r = alpha_r,
                            alpha_f = alpha_f,
                            psi_r = psi_r,
                            psi_f = psi_f,
                            lambda_r = lambda_r,
                            lambda_f = lambda_f,
                            nu_r = nu_r,
                            nu_f = nu_f,
                            Theta_r = Theta_r,
                            Theta_f = Theta_f,
                            pmix_ref = pmix_ref, 
                            plot_contour = plot_contour,
                            labels = c("Reference", "Focal"),
                            show_mi_result = show_mi_result) 
  class(store_par[[1]]) <- "PartInv"
  
  partial <- store_par[[1]]$summary
  strict  <- store_str[[1]]$summary
  
  # Compare accuracy indices for reference and focal groups under strict vs. 
  # partial invariance conditions and compute h for full item set
  acc <- acc_indices_h(store_str[[1]], store_par[[1]])
  
  str_par_ref_list[[1]] <- acc$Reference
  str_par_foc_list[[1]] <- acc$Focal
  
  h_str_vs_par_ref[1] <-acc$Reference$h 
  h_str_vs_par_foc[1] <-acc$Focal$h 
  
  h_R_Ef[1] <- cohens_h(partial$Reference, partial$`E_R(Focal)`)
  
  # Re-weight SE, SR, SP by focal and group proportions to compute 
  # aggregate indices under partial invariance for the full item set
  aggregate_par[1] <- get_composite_CAI(pmix_ref, partial) 
  # Repeat for strict invariance
  aggregate_str[1] <-  get_composite_CAI(pmix_ref, strict)
  
  # Compute h for the difference between strict and partial invariance for 
  # aggregate SE, SR, SP
  h_aggregate_str_par[1] <- cohens_h(aggregate_str[1], aggregate_par[1])
  AI_ratios[,1] <- c(store_str[[1]]$ai_ratio, store_par[[1]]$ai_ratio) 
  # (Re)set the proportion selected based on the PartInv output when all
  # items are included in the strict invariance condition
  propsel <- store_str[[1]]$propsel
  cut_z <- NULL
  
  # Item deletion scenarios
  for (i in seq_len(length(weights_item) + 1)[-1]) {
    
    # Assign a weight of 0 to the item to be deleted (indexed at i - 1), and 
    # redistribute the weight from this item across the non-deleted items
    take_one_out <- redistribute_weights(weights_item, n_dim = n_dim,
                                         n_i_per_dim = n_i_per_dim, 
                                         del_i = i - 1)
    
    # Call PartInv with the new weights under strict invariance
    store_str[[i]] <- PartInv(propsel, 
                              cut_z = cut_z,
                              take_one_out, 
                              weights_latent,
                              alpha_r = alpha_r,
                              alpha_f = alpha_f,
                              psi_r = psi_r,
                              psi_f = psi_f,
                              lambda_r = lambda_f * (1 - pmix_ref) + 
                                lambda_r * pmix_ref,
                              nu_r = nu_f * (1 - pmix_ref) + 
                                nu_r * pmix_ref,
                              Theta_r = Theta_f * (1 - pmix_ref) + 
                                Theta_r * pmix_ref,
                              pmix_ref = pmix_ref, 
                              plot_contour = plot_contour,
                              labels = c("Reference", "Focal"),
                              show_mi_result = show_mi_result)
    class(store_str[[i]]) <- "PartInv"
    # Call PartInv with the new weights under partial invariance
    store_par[[i]] <- PartInv(propsel, 
                              cut_z = cut_z,
                              take_one_out, 
                              weights_latent,
                              alpha_r = alpha_r,
                              alpha_f = alpha_f,
                              psi_r = psi_r,
                              psi_f = psi_f,
                              lambda_r = lambda_r,
                              nu_r = nu_r,
                              nu_f = nu_f,
                              Theta_r = Theta_r,
                              Theta_f = Theta_f,
                              pmix_ref = pmix_ref, 
                              plot_contour = plot_contour,
                              labels = c("Reference", "Focal"),
                              show_mi_result = show_mi_result)
    class(store_par[[i]]) < "PartInv"
    partial <- store_par[[i]]$summary
    strict <- store_str[[i]]$summary
    
    # Compute h for the difference in accuracy indices for reference and focal 
    # groups under strict vs. partial invariance conditions 
    acc_del_i <- acc_indices_h(store_str[[i]], store_par[[i]])
    str_par_ref_list[[i]] <- acc_del_i$Reference
    str_par_foc_list[[i]] <- acc_del_i$Focal
    
    h_str_vs_par_ref[i] <- str_par_ref_list[[i]]$h
    h_str_vs_par_foc[i] <- str_par_foc_list[[i]]$h
    
    # Compute the change in Cohen's h comparing accuracy indices for the 
    # reference and focal groups under strict vs. partial invariance when item i
    # is deleted i.e. the change in h_str_vs_par_ref and h_str_vs_par_foc
    delta_h_str_vs_par_ref[i - 1] <- delta_h(h_str_vs_par_ref[1], 
                                             h_str_vs_par_ref[i])
    delta_h_str_vs_par_foc[i - 1] <- delta_h(h_str_vs_par_foc[1], 
                                             h_str_vs_par_foc[i])
    # Compute h for the difference in accuracy indices under partial invariance
    # for the reference group vs. for the expected accuracy indices for the 
    # focal group if it followed the same distribution as the reference group 
    # (`E_R(Focal)`)
    h_R_Ef[i] <- cohens_h(partial$Reference, partial$`E_R(Focal)`)
    
    # Compute the change in Cohen's h comparing accuracy indices under partial
    # invariance for the reference group vs. for the expected accuracy indices
    # for the focal group if it followed the same distribution as the reference
    # group when item i is deleted, i.e. the change in h_R_Ef_del
    delta_h_R_vs_Ef[i-1] <- delta_h(h_R_Ef[1], h_R_Ef[i])
    
    # Compute aggregate SR, SE, SP indices under partial invariance by weighting 
    # accuracy indices for the reference and focal groups by their group
    # proportions
    aggregate_par[i] <- get_composite_CAI(pmix_ref, partial)
    # Repeat for strict invariance
    aggregate_str[i] <- get_composite_CAI(pmix_ref, strict)
    # Compute Cohen's h for the difference between aggregate SE, SR, SP under 
    # strict vs. partial invariance
    h_aggregate_str_par[i] <- cohens_h(aggregate_str[i], aggregate_par[i])
    # Compute how aggregate SR, SE, SP indices change when an item is deleted
    # under partial invariance
    h_aggregate_par[i - 1] <- cohens_h(aggregate_par[1], aggregate_par[i])
    
    delta_h_str_par_aggregate[i - 1] <- delta_h(h_aggregate_str_par[1], 
                                                    h_aggregate_str_par[i])
    
    AI_ratios[,i] <- c(store_str[[i]]$ai_ratio, store_par[[i]]$ai_ratio) 
  }
  # Format stored variables
  # names(AI_ratios) <- c("AI", paste0("AI|", c(1:N)))
  # rownames(AI_ratios) <- c("SFI", "PFI")
  names(AI_ratios) <- c("full", paste0("|", c(1:N)))
  rownames(AI_ratios) <- c("AI_SFI", "AI_PFI")
  # h_R_Ef
  # names(h_R_Ef) <-  c("h(r-Ef)", paste0("h(r-Ef|", c(1:N), c(")")))
  names(h_R_Ef) <-  c("r-Ef", paste0("r-Ef|", c(1:N)))
  
  
  # delta_h_str_vs_par
  # names(delta_h_str_vs_par_ref) <- names(delta_h_str_vs_par_foc) <- 
  #   paste0('\u0394', "h(SFI, PFI|", c(1:N), c(")"))
  names(delta_h_str_vs_par_ref) <- names(delta_h_str_vs_par_foc) <- 
       paste0("SFI, PFI|", c(1:N))
  
  names(store_str) <- names(store_par) <- 
    names(str_par_ref_list) <- names(str_par_foc_list) <- 
    c("full", paste0("|", c(1:N)))
  names(aggregate_par) <- c("full", paste0("|", c(1:N)))
  
  #  names(h_aggregate_par) <- paste0("h(|", c(1:N), c(")"))
  names(h_aggregate_par) <- paste0("|", c(1:N))
  
  # rownames(h_aggregate_par) <- rownames(aggregate_par) <- 
  #  rownames(delta_h_str_par_aggregate) <- rownames(h_aggregate_str_par) <- 
  #  c("PS*", "SR*", "SE*", "SP*")
  rownames(h_aggregate_par) <- rownames(h_aggregate_str_par) <-c("h(PS*)", "h(SR*)", "h(SE*)", "h(SP*)")
    rownames(aggregate_par) <-   c("PS*", "SR*", "SE*", "SP*")
   rownames(delta_h_str_par_aggregate) <-  c(paste0('\u0394', c("h(PS*)")), 
                                           paste0('\u0394', c("h(SR*)")),
                                           paste0('\u0394', c("h(SE*)")), 
                                           paste0('\u0394', c("h(SP*)")))
  
  # rownames(delta_h_str_vs_par_ref) <- rownames(delta_h_str_vs_par_foc) <-
  #   rownames(h_R_Ef) <- rownames(delta_h_R_vs_Ef) <- 
  #   rownames(h_str_vs_par_ref) <- rownames(h_str_vs_par_foc) <- 
  #   c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  
   
   rownames(delta_h_str_vs_par_ref) <- rownames(delta_h_str_vs_par_foc) <-
     rownames(delta_h_R_vs_Ef) <-  c(paste0('\u0394', c("h(TP)")), 
                                     paste0('\u0394', c("h(FP)")),
                                     paste0('\u0394', c("h(TN)")), 
                                     paste0('\u0394', c("h(FN)")),
                                     paste0('\u0394', c("h(PS)")), 
                                     paste0('\u0394', c("h(SR)")),
                                     paste0('\u0394', c("h(SE)")), 
                                     paste0('\u0394', c("h(SP)")))
   
     rownames(h_R_Ef) <- rownames(h_str_vs_par_ref) <- rownames(h_str_vs_par_foc) <- 
     c("h(TP)", "h(FP)", "h(TN)", "h(FN)", "h(PS)", "h(SR)", "h(SE)", "h(SP)")
   
  # Declare classes
  store_par <- list(outputlist = store_par, condition = "partial",
                    itemset = return_items)
  store_str <- list(outputlist = store_str, condition = "strict", 
                    itemset = return_items)
  class(store_par) <- c("PartInvList", "PartInv")
  class(store_str) <- c("PartInvList", "PartInv")
  h_str_vs_par_list_ref <- list(outputlist = str_par_ref_list, condition="ref",
                                itemset = return_items)
  h_str_vs_par_list_foc <- list(outputlist = str_par_foc_list, condition="foc",
                                itemset = return_items)
  class(h_str_vs_par_list_ref) <- "PartInvList"
  class(h_str_vs_par_list_foc) <- "PartInvList"
  
  
  # names(h_str_vs_par_ref) <- names(h_str_vs_par_foc) <- c("h(SFI, PFI)", 
  #                                                         paste0("h(SFI, PFI|", c(1:N), c(")")))
  names(h_str_vs_par_ref) <- names(h_str_vs_par_foc) <- c("SFI, PFI", 
                                                          paste0("SFI, PFI|", c(1:N)))
  
  # names(delta_h_R_vs_Ef) <- paste0('\u0394', "h(r-Ef|", c(1:N), c(")"))
  # names(delta_h_str_par_aggregate) <- paste0('\u0394', "h(SFI, PFI|", c(1:N), c(")"))
  # names(h_aggregate_str_par) <- c("h(SFI, PFI)", paste0("h(SFI, PFI|", c(1:N), c(")")))
  # 
  names(delta_h_R_vs_Ef) <- paste0("r-Ef|", c(1:N))
  names(delta_h_str_par_aggregate) <- paste0( "SFI, PFI|", c(1:N))
  names(h_aggregate_str_par) <- c("SFI, PFI", paste0("SFI, PFI|", c(1:N)))
  
  
  aggregate_par <- as.data.frame(cbind(aggregate_par))
  h_aggregate_par <- as.data.frame(h_aggregate_par)
  
  h_str_vs_par <- list("ref"= t(h_str_vs_par_ref), "foc" = t(h_str_vs_par_foc))
  
  h_aggregate_str_par <- h_aggregate_str_par
  
  returned <- list(
    "h_aggregate_par" = t(h_aggregate_par),
    "delta_h_str_par_aggregate" = t(delta_h_str_par_aggregate),
    "AI Ratio" = t(AI_ratios),
    "h_R_Ef" = t(h_R_Ef),
    "h_R_vs_Ef.par" = t(delta_h_R_vs_Ef),
    "delta_h_str_vs_par" = list("ref" = t(delta_h_str_vs_par_ref), 
                                "foc" = t(delta_h_str_vs_par_foc)), 
    "str_vs_par" = h_str_vs_par,
    "aggregate_par" = t(aggregate_par),
    "h_aggregate_str_par" = t(h_aggregate_str_par),
    "Ref_foc" = list("reference" = h_str_vs_par_list_ref, 
                     "focal" = h_str_vs_par_list_foc),
    "PartInv" = list("strict" = store_str,
                     "partial" = store_par),
    "detail" = return_detailed,
    "return_items" = return_items)
  class(returned) <- "itemdeletion"
  
  # NOTE TO SELF: make return_items a part of the class definition, final change 
  # should be made there
  
  return(returned)
}

