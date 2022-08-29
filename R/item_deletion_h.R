#' @title
#' Impact of deleting biased item(s) on classification accuracy indices
#' 
#' @name
#' item_deletion_h
#' 
#' @description
#' \code{item_deletion_h} computes effect size indices that quantify the impact 
#'  of (and changes in the impact of) measurement bias on classification accuracy 
#'  indices (CAI) such as TP and SE if an item is dropped vs. included in analyses. 
#'  Comparisons are made between CAI computed for the reference group and 
#'  expected CAI computed for the focal group; between CAI computed under strict 
#'  factorial invariance (SFI) vs. CAI computed under partial factorial 
#'  invariance (PFI); and between aggregate CAI computed for item subsets.
#' @param propsel Proportion of selection. If missing, computed using `cut_z`.
#' @param cut_z Pre-specified cutoff score on the observed composite. This 
#'        argument is ignored when `propsel` has an input.
#' @param weights_item A vector of item weights.
#' @param weights_latent A  vector of latent factor weights.
#' @param alpha_r A vector of latent factor mean for the reference group.
#' @param alpha_f (optional) A vector of latent factor mean for the focal  
#'        group; if no input, set equal to `alpha_r`.
#' @param psi_r A matrix of latent factor variance covariances for the reference
#'        group.
#' @param psi_f (optional) A matrix of latent factor variance-covariances for 
#'        the focal group; if no input, set equal to `psi_r`.
#' @param lambda_r A matrix of factor loadings for the reference group.
#' @param lambda_f (optional) A matrix of factor loadings for the focal group; 
#'        if no input, set equal to `lambda_r`.
#' @param nu_r A matrix of measurement intercepts for the reference group 
#'        under the partial invariance condition.
#' @param nu_f (optional) A matrix of measurement intercepts for the focal 
#'        group; if no input, set equal to `nu_r`.
#' @param Theta_r A matrix of the unique factor variances and covariances 
#'        for the reference group.
#' @param Theta_f (optional) A matrix of the unique factor variances and 
#'        covariances for the focal group; if no input, set equal to `Theta_r`.
#' @param pmix_ref Proportion of the reference group; default to 0.5 (i.e., two 
#'        populations have equal size).
#' @param plot_contour Logical; whether the contour of the two populations 
#'        should be plotted; default to `TRUE`.
#' @param show_mi_result If \code{TRUE}, perform classification accuracy analysis
#'        for both the input parameters and the implied parameters based on a
#'        strict invariance model, with common parameter values as weighted
#'        averages of the input values using `pmix_ref`.
#' @param n_dim Number of dimensions, 1 by default. If the user does not supply 
#'        a different value, proceeds with the assumption that the scale is 
#'        unidimensional.
#' @param n_i_per_dim A vector containing the number of items in each 
#'        dimension; `NULL` by default. If the user provides a value for `n_dim` 
#'        that is \eqn{> 1} but leaves \code{n_i_per_dim = NULL}, assumes that 
#'        the subscales have an equal number of items. 
#' @param user_specified_items A vector; default to `NULL`. If the user does not
#'        input a vector of items, only the items determined to contain bias will
#'        be considered for deletion.
#' @param print_formatted Logical; default to `TRUE`. By default, prints five 
#'        formatted summary tables for biased (or user-specified) items: `ACAI`,
#'        `h ACAI (deletion)`, `AI Ratio`, `h CAI Ref-EF`, and 
#'        `delta h CAI Ref-EF (deletion)`. The formatted print output is separate 
#'        from the returned object. The function always returns an object with 
#'        detailed elements that can be accessed if the function output is stored 
#'        in a variable. If `print_formatted==FALSE`, all elements in the 
#'        returned object are printed without formatting.
#' @param delete_one_cutoff (optional) User-specified cutoff to use in
#'        delete-one scenarios. `NULL` by default; if `NULL`, proportion 
#'        selected under SFI and PFI when the full item set is used is passed 
#'        onto calls to PartInv.
#' @param labels A character vector with two elements to label the reference
#'     and the focal group on the graph.
#' @param ... Other arguments passed to the \code{\link[graphics]{contour}}
#'     function.
#' @return An object of class `itemdeletion` containing 13 elements. 
#'        \item{ACAI}{A matrix that stores aggregate PS, SR, SE, SP computed for
#'        the full set of items and item subsets excluding biased or user specified
#'        items under PFI.}
#'        \item{h ACAI (deletion)}{A matrix that stores Cohen's h computed for 
#'        the impact of deleting each item considered in the `ACAI` table.}
#'        \item{h ACAI SFI-PFI}{A matrix that stores Cohen's h values 
#'        quantifying the discrepancy between ACAI under SFI vs. ACAI under PFI.}
#'        \item{delta h ACAI SFI-PFI (deletion)}{A matrix that stores delta h 
#'        values quantifying the impact of deleting an item on the discrepancy 
#'        between ACAI under SFI vs. ACAI under PFI for subsets of items.}
#'        \item{AI Ratio}{A matrix storing Adverse Impact Ratio values computed
#'        for item subsets by invariance condition.}
#'        \item{h CAI Ref-EF}{A matrix that stores Cohen's h values quantifying
#'        the discrepancy between CAI computed for the reference group and the 
#'        expected CAI computed for the focal group if it matched the 
#'        distribution of the reference group (Efocal), under PFI for subsets of
#'        items.}
#'        \item{delta h CAI Ref-EF (deletion)}{A matrix that stores delta h 
#'        values quantifying the impact of deleting an item on the discrepancy 
#'        between CAI of reference vs. Efocal groups under PFI.}
#'        \item{h CAI SFI-PFI}{A list containing two items, `ref` and `foc` 
#'        which are matrices storing Cohen's h values quantifying the 
#'        discrepancy between CAI under SFI vs. PFI for the reference group and
#'        the focal group respectively, for subsets of items.}
#'        \item{delta h SFI-PFI (deletion)}{A list containing two items, 
#'        `ref` and `foc` which are matrices storing delta h values quantifying 
#'        the impact of deleting an item on the discrepancy between CAI under
#'        SFI vs. PFI for the reference group and the focal group respectively,
#'        for subsets of items.}
#'        \item{PartInv by groups}{Two lists (`reference` and `focal`), each 
#'        containing restructured tables of PartInv outputs by invariance
#'        condition, and corresponding Cohen's h values for each item deletion
#'        scenario.}
#'        \item{PartInv}{Two lists (`strict` and `partial`), each containing 
#'        PartInv() outputs.}
#'        \item{formatted}{Logical, whether simplified and formatted output  
#'        should be printed.}
#'        \item{return_items}{A vector containing the items that will be considered
#'        for deletion.}
#' @examples
#' # Multidimensional example
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' 
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
#'                              Theta_f = diag(c(1, .95, .80, .75, 1)),
#'                              plot_contour = TRUE,
#'                              print_formatted = TRUE)
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
#'                                print_formatted = TRUE)
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
                            print_formatted = TRUE,
                            user_specified_items = NULL,
                            delete_one_cutoff = NULL,
                            ...) {
  N <- length(weights_item)
  
  # Determine which set of items will be returned
  return_items <- c()
  if(is.null(user_specified_items)) { # default: return only the biased items.
      return_items <- determine_biased_items(lambda_r = lambda_r,
                                             lambda_f = lambda_f, 
                                             nu_r = nu_r, nu_f = nu_f, 
                                             Theta_r = Theta_r,
                                             Theta_f = Theta_f, weights_item)
  } else {
    if (!all(user_specified_items == floor(user_specified_items))) { 
      stop("'user_specified_items' should only contain integers corresponding 
             to item indices.")}
    if (!all(user_specified_items < N + 1)) { 
      stop("'user_specified_items' cannot take integers larger than the scale
             length.")}
    return_items <- user_specified_items
  }
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
                            show_mi_result = show_mi_result)
  
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
  aggregate_par[1] <- get_aggregate_CAI(pmix_ref, partial) 
  # Repeat for strict invariance
  aggregate_str[1] <-  get_aggregate_CAI(pmix_ref, strict)
  
  # Compute h for the difference between strict and partial invariance for 
  # aggregate SE, SR, SP
  h_aggregate_str_par[1] <- cohens_h(aggregate_str[1], aggregate_par[1])
  AI_ratios[,1] <- c(store_str[[1]]$ai_ratio, store_par[[1]]$ai_ratio) 
  
  # If the user supplied a new cutoff, set cut_z to that and set propsels to NULL.
  # If no cutoff was inputted, set propsel based on PartInv output with all items
  if(is.null(delete_one_cutoff)) {
    propsel_p <- store_par[[1]]$propsel
    propsel_s <- store_str[[1]]$propsel
    cut_z <- NULL
  } else {
    cut_z <- delete_one_cutoff
    propsel_p <- NULL
    propsel_s <- NULL
  }
  
  # Item deletion scenarios
  for (i in seq_len(length(weights_item) + 1)[-1]) {
    
    # Assign a weight of 0 to the item to be deleted (indexed at i - 1), and 
    # redistribute the weight from this item across the non-deleted items
    take_one_out <- redistribute_weights(weights_item, n_dim = n_dim,
                                         n_i_per_dim = n_i_per_dim, 
                                         del_i = i - 1)
    # Call PartInv with the new weights under strict invariance
    store_str[[i]] <- PartInv(propsel_s, 
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
    store_par[[i]] <- PartInv(propsel_p, 
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
    
    # Check whether improvements in ACAI may be misleading due pmix_ref
    err_improv_acai(i = i, store_summary_full = store_par[[1]]$summary,
                    store_summary_del1 = store_par[[i]]$summary)
    err_improv_acai(i = i, store_summary_full = store_str[[1]]$summary,
                    store_summary_del1 = store_str[[i]]$summary)
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
    aggregate_par[i] <- get_aggregate_CAI(pmix_ref, partial)
    # Repeat for strict invariance
    aggregate_str[i] <- get_aggregate_CAI(pmix_ref, strict)
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
  class(h_str_vs_par_list_ref) <-  c("PartInvList", "PartInv", "PartInv_groups" )
  class(h_str_vs_par_list_foc) <-  c("PartInvList", "PartInv", "PartInv_groups")
  
  
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
    "ACAI" = t(aggregate_par),
    "h ACAI (deletion)" = t(h_aggregate_par),
    "h ACAI SFI-PFI" = t(h_aggregate_str_par),
    "delta h ACAI SFI-PFI (deletion)" = t(delta_h_str_par_aggregate),
    "AI Ratio" = t(AI_ratios),
    "h CAI Ref-EF" = t(h_R_Ef),
    "delta h CAI Ref-EF (deletion)" = t(delta_h_R_vs_Ef),
    "h CAI SFI-PFI" = h_str_vs_par,
    "delta h SFI-PFI (deletion)" = list("ref" = t(delta_h_str_vs_par_ref), 
                                "foc" = t(delta_h_str_vs_par_foc)), 
    "PartInv by groups" = list("reference" = h_str_vs_par_list_ref, 
                     "focal" = h_str_vs_par_list_foc),
    "PartInv" = list("strict" = store_str,
                     "partial" = store_par),
    "formatted" = print_formatted,
    "return_items" = return_items)
  class(returned) <- "itemdeletion"
  
  return(returned)
}

