
# Function that performs formatting for various variables to be returned in
# item_deletion_h
format_item_del <- function(N, l) {
  # Format stored variables
  names(l$AI_ratios) <- c("full", paste0("|", c(1:N)))
  rownames(l$AI_ratios) <- c("AI_SFI", "AI_PFI")
  names(l$h_R_Ef) <-  c("r-Ef", paste0("r-Ef|", c(1:N)))
  names(l$delta_s_p_ref) <- names(l$delta_s_p_foc) <- paste0("SFI, PFI|", 
                                                             c(1:N))
  names(l$store_str) <- names(l$store_par) <- names(l$s_p_ref_list) <-
    names(l$s_p_foc_list) <- c("full", paste0("|", c(1:N)))
  names(l$acai_p) <- c("full", paste0("|", c(1:N)))
  names(l$h_acai_p) <- paste0("|", c(1:N))
  rownames(l$h_acai_p) <- rownames(l$h_acai_s_p) <-
    c("h(PS*)", "h(SR*)", "h(SE*)", "h(SP*)")
  rownames(l$acai_p) <- c("PS*", "SR*", "SE*", "SP*")
  rownames(l$delta_h_s_p_acai) <-
    paste0("\u0394h(", c("h(PS*)", "h(SR*)", "h(SE*)", "h(SP*)"), ")")
  rownames(l$delta_s_p_ref) <- rownames(l$delta_s_p_foc) <- 
    rownames(l$delta_h_R_Ef) <-
    paste0("\u0394h(", c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP"), ")")
  rownames(l$h_R_Ef) <- rownames(l$h_s_p_ref) <- rownames(l$h_s_p_foc) <-
    c("h(TP)", "h(FP)", "h(TN)", "h(FN)", "h(PS)", "h(SR)", "h(SE)", "h(SP)")
  
  store_par <- list(outputlist = l$store_par, condition = "partial",
                    itemset = l$return_items)
  store_str <- list(outputlist = l$store_str, condition = "strict",
                    itemset = l$return_items)
  h_s_p_list_ref <- list(outputlist = l$s_p_ref_list, condition = "ref",
                                itemset = l$return_items)
  h_s_p_list_foc <- list(outputlist = l$s_p_foc_list, condition = "foc",
                                itemset = l$return_items)
  names(l$h_s_p_ref) <- names(l$h_s_p_foc) <- c("SFI, PFI", 
                                            paste0("SFI, PFI|", c(1:N)))
  
  names(l$delta_h_R_Ef) <- paste0("r-Ef|", c(1:N))
  names(l$delta_h_s_p_acai) <- paste0("SFI, PFI|", c(1:N))
  names(l$h_acai_s_p) <- c("SFI, PFI", paste0("SFI, PFI|", c(1:N)))
  
  acai_p <- as.data.frame(cbind(l$acai_p))
  h_acai_p <- as.data.frame(l$h_acai_p)
  
  return(list("acai_p" = t(acai_p), "h_acai_p" = t(h_acai_p), 
              "h_acai_s_p" = t(l$h_acai_s_p), 
              "delta_h_s_p_acai" = t(l$delta_h_s_p_acai), 
              "AI_ratios" = t(l$AI_ratios), "h_R_Ef" = t(l$h_R_Ef), 
              "delta_h_R_Ef" = t(l$delta_h_R_Ef), "h_s_p_ref" = t(l$h_s_p_ref), 
              "h_s_p_foc" = t(l$h_s_p_foc), 
              "delta_s_p_ref" = t(l$delta_s_p_ref), 
              "delta_s_p_foc" = t(l$delta_s_p_foc), 
              "h_s_p_list_ref" = h_s_p_list_ref, 
              "h_s_p_list_foc" = h_s_p_list_foc, 
              "store_str" = store_str, "store_par" = store_par))
}



#' @title
#' Compute PS, SR, SE, SP weighted by group proportions
#'
#' @name
#' get_aggregate_CAI
#'
#' @description
#' \code{get_aggregate_CAI} computes aggregate PS, SR, SE, SP under partial or
#' strict invariance by weighting the TP, TF, TN, FP values for the reference
#' and focal groups with the group proportions.
#'
#' @param pmixr Proportion of the reference group.
#' @param store_summary The summary table from [PartInv()]
#'   under partial or strict invariance.
#'
#' @return A vector of length 4.
#'          \item{PS}{Proportion selected, computed as \eqn{TP + FP}.}
#'          \item{SR}{Success ratio, computed as \eqn{TP/(TP + FP)}.}
#'          \item{SE}{Sensitivity, computed as \eqn{TP/(TP + FN)}.}
#'          \item{SP}{Specificity, computed as \eqn{TN/(TN + FP)}.}

get_aggregate_CAI <- function(pmixr, store_summary) {
  r <- store_summary$Reference
  f <- store_summary$Focal
  pmixf <- 1 - pmixr
  
  TP <- pmixr * r[1] + pmixf * f[1]
  FP <- pmixr * r[2] + pmixf * f[2]
  TN <- pmixr * r[3] + pmixf * f[3]
  FN <- pmixr * r[4] + pmixf * f[4]
  
  PS <- TP + FP
  SR <- TP / (TP + FP)
  SE <- TP / (TP + FN)
  SP <- TN / (TN + FP)
  return(c(PS, SR, SE, SP))
}

#' @title
#' Check for misleading improvements in aggregate CAI
#'
#' @name
#' err_improv_acai
#'
#' @description
#' \code{err_improv_acai} checks if any improvement observed in aggregate CAI
#' may have resulted from the higher mixing proportion of the reference group
#' masking worsening performance for the focal group. If the effect size of any
#' change indicating worse performance for the focal group and better
#' performance for the reference group is larger than 0.1, prints a warning
#' message.
#' @param i Index of item under consideration.
#' @param store_summary_full PartInv summary for the case where all items are
#'   retained.
#' @param store_summary_del1 PartInv summary for the case where item i is
#'   excluded.
err_improv_acai <- function(i, store_summary_full, store_summary_del1) {
  # Store relevant values.
  r <- store_summary_full$Reference
  f <- store_summary_full$Focal
  r_del1 <- store_summary_del1$Reference
  f_del1 <- store_summary_del1$Focal

  # Compute Cohen's h for the difference between full and drop one indices.
  h_r <- cohens_h(r, r_del1)
  h_f <- cohens_h(f, f_del1)
  # Check for changes (boolean).
  r_bool <- r < r_del1 
  f_bool_leq <- f <= f_del1
  f_bool_geq <- f >= f_del1
  # Check the difference for the reference or focal group has Cohen's h > 0.1.
  h_rf.1 <- (h_r > 0.1 | h_f > 0.1)

  vals <- c("TP", "FP", "TN", "FN")

  # TP_f decreases/remains unchanged & TP_r increases
  if(r_bool[1] && f_bool_geq[1] && h_rf.1[1]) {
    cat1(1, vals, 1)
    }
  # FP_r decreases and FP_f increases/remains unchanged
  if(!r_bool[2] && f_bool_leq[2] && h_rf.1[2]) {
    cat1(2, vals, 2)
    }
  # TN_f decreases/remains unchanged and TN_r increases
  if(r_bool[3] && f_bool_geq[3] && h_rf.1[3]) {
    cat1(3, vals, 3)
    }
    # FN_r decreases and FN_f increases/remains unchanged
  if(!r_bool[4] && f_bool_leq[4] && h_rf.1[4]) {
    cat1(4, vals, 4)
    }

  cat1 <- function(i, vals, val_i) {
    cat("Increases in aggregate CAI after deleting item ", i, "may be
         misleading due to the \n mixing proportion. Examine ", vals[val_i],
        "values from detailed output tables before proceeding.\n")
    }
  }

#' @title
#' Delete item i and redistribute its weight within subscale
#'
#' @name
#' redistribute_weights
#'
#' @description
#' \code{redistribute_weights} replaces the item weight with 0 for the item to
#' be deleted, and redistributes this item's weight across the remaining items.

#' @param weights_item A vector of item weights.
#' @param n_dim Number of dimensions, 1 by default. If the user does not supply
#'   a value, assumes that the scale is unidimensional.
#' @param n_i_per_dim A vector containing the number of items in each
#'   dimension; `NULL` by default. If the user provides a value for n_dim
#'   that is \eqn{> 1} but leaves \code{n_i_per_dim = NULL}, assumes that
#'   the subscales have an equal number of items.
#' @param del_i Index of the item to be deleted.
#'
#' @return `new_w` Weights vector with redistributed weights.
#' @examples
#' one_dim_w <- c(1:7)
#' redistribute_weights(one_dim_w, del_i = 2)
#' redistribute_weights(one_dim_w, n_dim = 1, n_i_per_dim = 7, del_i = 2)
#' sum(one_dim_w)==sum(redistribute_weights(one_dim_w, del_i = 2))
#'
#' multi_eq_w <- c(1:9)
#' redistribute_weights(multi_eq_w, n_dim = 3, del_i = 2)
#' redistribute_weights(multi_eq_w, n_dim = 3, n_i_per_dim = c(3, 3, 3), 
#' del_i = 2)
#' sum(multi_eq_w)==sum(redistribute_weights(multi_eq_w, n_dim = 3, del_i = 2))
#'
#' multi_uneq_w <- c(1:12)
#' redistribute_weights(multi_uneq_w, n_dim = 3, n_i_per_dim = c(3, 6, 3), 
#' del_i=2)
#' sum(multi_uneq_w)==sum(redistribute_weights(multi_uneq_w, n_dim = 3,
#'                                             n_i_per_dim = c(3, 6, 3),
#'                                             del_i=2))
#' @export
redistribute_weights <- function(weights_item, n_dim = 1, n_i_per_dim = NULL,
                               del_i) {
  n_items <- length(weights_item)
  new_w <- weights_item
  new_w[del_i] <- 0
  del_weight <- weights_item[del_i] # the weight to be redistributed

  # Unidimensional
  if ((n_dim == 1) && (is.null(n_i_per_dim) || length(n_i_per_dim) == 1)) {
    # Increase each non-zero item in the vector by the weight to be distributed
    # proportional to the original weighting of the items.
    new_w[new_w != 0] <- new_w[new_w != 0] +
      new_w[new_w != 0] * del_weight / sum(new_w[new_w != 0])

  # Multidimensional, equal n
  } else if ((n_dim > 1) && is.null(n_i_per_dim) && n_items %% n_dim != 0) {
    stop("Please pass a vector of subscale lengths to n_i_per_dim.")

  # Multidimensional, number of items per dimension is not specified
  } else if ((n_dim > 1) && is.null(n_i_per_dim)) {
    # Split indices into dimensions assuming dimensions have the same length.
    i_by_dim <- split(1:n_items, cut(seq_along(1:n_items), n_dim, 
                                     labels = FALSE))
    new_w <- multidim_redist(n_dim, del_i, i_by_dim, new_w, del_weight)

  # Multidimensional, unequal n
  } else if ((n_dim > 1) && !is.null(n_i_per_dim)) {
    # Split indices into dimensions
    i_by_dim <- split(1:n_items, cut(seq_along(1:n_items),
                                     breaks = cumsum(c(0, n_i_per_dim)),
                                     labels = FALSE))
    new_w <- multidim_redist(n_dim, del_i, i_by_dim, new_w, del_weight)
  } else {
    stop("Check n_dim and n_i_per_dim")
  }
  return(new_w)
}
# Helper function for redistribute_weights() that, for each dimension of the
# scale, updates the specific subscale with redistributed weights proportional
# to the original weights if the item to be deleted is in that dimension.
multidim_redist <- function(n_dim, del_i, i_by_dim, new_w, del_weight) {
  for (k in 1:n_dim) {
    if (del_i %in% i_by_dim[[k]]) { # If del_i is in dimension k
      # Create temporary vector to store the remaining indices in dimension k
      temp_i <- i_by_dim[[k]][i_by_dim[[k]] != del_i]
      new_w[temp_i][new_w[temp_i] != 0] <- new_w[temp_i][new_w[temp_i] != 0] +
        new_w[temp_i][new_w[temp_i] != 0] * del_weight / sum(new_w[temp_i])
    }
  }
  return(new_w)
}

#' @title
#' Compute Cohen's h effect size for the difference in two proportions.
#'
#' @name
#' cohens_h
#'
#' @description
#' \code{cohens_h} computes Cohen's h (Cohen, 1988) for the difference in two
#' proportions using \eqn{h = 2arcsin(\sqrt{p1}) - 2arcsin(\sqrt{p2})}.
#'
#' @param p1 The first proportion.
#' @param p2 The second proportion.
#' @return `h` The computed Cohen's h value.
#' @examples
#' cohens_h(0.7, 0.75)
#' cohens_h(0.3, 0.4)
#' @export
cohens_h <- function(p1, p2) {
  h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  return(h)
}

#' @title
#' Compute effect size for the impact of item deletion
#'
#' @name
#' delta_h
#'
#' @description
#' \code{delta_h} Computes the effect size of the impact of item bias by
#' comparing Cohen's h values for CAI for the full versus delete-one item sets.
#'
#' @param h_R h effect sizes for when the item is included.
#' @param h_i_del h effect sizes for when the item is deleted.
#' @return Cohen's h for the difference in the classification accuracy index
#'   when the item is deleted.
#' @examples
#' delta_h(0.04, 0.01)
#' delta_h(-0.002, 0.011)
#' @export
delta_h <- function(h_R, h_i_del) {
  abs(h_R) - abs(h_i_del)
}


#' @title
#' Compute Cohen's h for the difference under strict vs. partial invariance for
#' the reference and the focal group.
#'
#' @name
#' acc_indices_h
#'
#' @description
#' \code{acc_indices_h} takes in outputs from [PartInv()]
#' and returns two restructured data frames with the classification accuracy
#' indices for the reference and focal groups under strict invariance and
#' partial invariance conditions, and the corresponding h for the difference in
#' CAI between the two invariance conditions for each group.
#' @param strict_output Output from [PartInv()] under strict invariance.
#' @param partial_output Output from [PartInv()] under partial invariance.
#' @return A 8 x 3 dataframe with columns `strict invariance`,
#'   `partial invariance`, and `h`.
acc_indices_h <- function(strict_output, partial_output) {
  r_names <- c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")

  ref_par_strict <- partial_output$summary[1][, 1]
  ref_strict <- strict_output$summary[1][, 1]

  df_ref <- data.frame(SFI =  ref_strict,
                   PFI = ref_par_strict, row.names = r_names)
  df_ref["h"] <- cohens_h(df_ref$SFI, df_ref$PFI)

  f_par_strict <- partial_output$summary[2][, 1]
  f_strict <- strict_output$summary[2][, 1]
  df_f <- data.frame(SFI =  f_strict,
                       PFI = f_par_strict, row.names = r_names)
  df_f["h"] <- cohens_h(df_f$SFI, df_f$PFI)
  return(list("Reference" = df_ref, "Focal" = df_f))
}


#' @title
#' Determine biased items
#'
#' @name
#' determine_biased_items
#'
#' @description
#' \code{determine_biased_items} takes in the factor loadings, intercepts, and
#'  uniqueness for the reference and focal groups, and returns indices of
#'  noninvariant items.
#'
#' @param lambda_r Factor loadings for the reference group.
#' @param lambda_f Factor loadings for the focal group.
#' @param nu_r Measurement intercepts for the reference group.
#' @param nu_f Measurement intercepts for the focal group.
#' @param Theta_r Uniqueness for the reference group.
#' @param Theta_f Uniqueness for the focal group.
#' @return A vector containing the indices of the biased items.
#' @examples
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' determine_biased_items(lambda_r = lambda_matrix,
#'                        lambda_f = lambda_matrix,
#'                        nu_r = c(.225, .025, .010, .240, .125),
#'                        nu_f = c(.225, -.05, .240, -.025, .125),
#'                        Theta_r = diag(1, 5),
#'                        Theta_f = diag(c(1, .95, .80, .75, 1)))
#' @export
determine_biased_items <- function(lambda_r, lambda_f, nu_r, nu_f,
                                   Theta_r, Theta_f) {
  biased_items <- c()
  # Compare factor loadings
  lambda_mismatch <- !(lambda_r == lambda_f)
  if (any(lambda_mismatch, TRUE)) {
    biased_items <- c(biased_items, which(lambda_mismatch))
    }
  # Compare uniqueness
  theta_mismatch <- !apply(Theta_r == Theta_f, 1, all)
  if (any(theta_mismatch, TRUE)) {
    biased_items <- c(biased_items, which(theta_mismatch)) 
    }
  # Compare intercepts
  nu_mismatch <- !(nu_r == nu_f)
  if (any(nu_mismatch, TRUE)) {
    biased_items <- c(biased_items, which(nu_mismatch)) }

  biased <- unique(biased_items)
  if (length(biased) == 0) {
    print("Strict invariance holds for all items.") 
    }
  return(sort(biased))
  }
