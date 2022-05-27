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
#' under partial or strict invariance.
#' 
#' @return A vector of length 4. 
#'          \item{PS}{Proportion selected, computed as \eqn{TP + TN}.}
#'          \item{SR}{Success ratio, computed as \eqn{TP/(TP + FP)}.}
#'          \item{SE}{Sensitivity, computed as \eqn{TP/(TP + FN)}.}
#'          \item{SP}{Specificity, computed as \eqn{TN/(TN + FP)}.}

get_aggregate_CAI <- function(pmixr, store_summary) {
  r <- store_summary$Reference; f <- store_summary$Focal
  PS <- (pmixr*r[1] + (1 - pmixr)*f[1]) + (pmixr*r[2] + (1 - pmixr)*f[2])
  SR <- (pmixr*r[1] + (1 - pmixr)*f[1]) /
    (pmixr*r[1] + (1 - pmixr)*f[1] + pmixr*r[2] + (1 - pmixr)*f[2]) 
  SE <- (pmixr*r[1] + (1 - pmixr)*f[1]) / 
    (pmixr*r[1] + (1 - pmixr)*f[1] + pmixr*r[4] + (1 - pmixr)*f[4])
  SP <- (pmixr*r[3] + (1 - pmixr)*f[3]) / 
    (pmixr*r[3] + (1 - pmixr)*f[3] + pmixr*r[2] + (1 - pmixr)*f[2]) 
  return(c(PS, SR, SE, SP))
}

#' @title
#' Check if improvements in ACAI may be misleading.x
#' 
#' @name 
#' err_improv_acai
#' 
#' @description
#' \code{err_improv_acai} Checks if any improvement observed in ACAI may have 
#' resulted from the higher mixing proportion of the reference group masking 
#' worsening performance for the focal group. If the effect size of any change 
#' indicating worse performance for the focal group and better performance for
#'  the reference group is larger than 0.1, prints a warning message.
#' @param i Index of item under consideration.
#' @param store_summary_full PartInv summary for the case where all items are 
#' retained.
#' @param store_summary_del1 PartInv summary for the case where item i is 
#' excluded.

err_improv_acai <- function(i, store_summary_full, store_summary_del1) {
  r <- round(store_summary_full$Reference, 3)
  f <- round(store_summary_full$Focal, 3)
  r_del1 <- round(store_summary_del1$Reference, 3)
  f_del1 <- round(store_summary_del1$Focal, 3)
  if(# TP_f decreases/remains unchanged and TP_r increases, and either change has Cohen's h > 0.1
    ((r[1] < r_del1[1]) & (f[1] >= f_del1[1]) & 
     (cohens_h(r[1], r_del1[1]) > 0.1 | cohens_h(f[1], f_del1[1]) > 0.1)) | 
    # FP_r decreases and FP_f increases/remains unchanged, and either change has Cohen's h > 0.1
    ((r[2] > r_del1[2]) & (f[2] <= f_del1[2]) & 
       (cohens_h(r[2], r_del1[2]) > 0.1 | cohens_h(f[2], f_del1[2]) > 0.1)) | 
    # TN_f decreases/remains unchanged and TN_r increases, and either change has Cohen's h > 0.1
    ((r[3] < r_del1[3]) & (f[3] >= f_del1[3]) & 
     (cohens_h(r[3], r_del1[3]) > 0.1 | cohens_h(f[3], f_del1[3]) > 0.1)) |  
    # FN_r decreases and FN_f increases/remains unchanged, and either change has Cohen's h > 0.1
    ((r[4] > r_del1[4]) & (f[4] <= f_del1[4]) & 
       (cohens_h(r[4], r_del1[4]) > 0.1 | cohens_h(f[4], f_del1[4]) > 0.1))
    )
  {
    if((r[1] < r_del1[1]) & (f[1] > f_del1[1]) & 
      (cohens_h(r[1 ], r_del1[1]) > 0.1|cohens_h(f[1],f_del1[1]) > 0.1)){
      cat("Increases in ACAI related to the deletion of item ", i, "may be misleading due to \nthe mixing proportion. Examine TP values from detailed output tables for the \nreference and focal groups before proceeding.\n")
      }
    if((r[2] > r_del1[2]) & (f[2] < f_del1[2]) & 
       (cohens_h(r[2 ], r_del1[2]) > 0.1|cohens_h(f[2],f_del1[2]) > 0.1)){
      cat("Increases in ACAI related to the deletion of item ", i, "may be misleading due to \nthe mixing proportion. Examine FP values from detailed output tables for the \nreference and focal groups before proceeding.\n")
      }
    if((r[3] < r_del1[3]) & (f[3] > f_del1[3]) & 
       (cohens_h(r[3 ], r_del1[3]) > 0.1|cohens_h(f[3],f_del1[3]) > 0.1)){
      cat("Increases in ACAI related to the deletion of item ", i, "may be misleading due to \nthe mixing proportion. Examine TN values from detailed output tables for the \nreference and focal groups before proceeding.\n")
      }
    if((r[4] > r_del1[4]) & (f[4] < f_del1[4]) & 
      (cohens_h(r[4 ], r_del1[4]) > 0.1|cohens_h(f[4],f_del1[4]) > 0.1)){
      cat("Increases in ACAI related to the deletion of item ", i, "may be misleading due to \nthe mixing proportion. Examine FN values from detailed output tables for the \nreference and focal groups before proceeding.\n")
    }
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
#'        a value, assumes that the scale is unidimensional.
#' @param n_i_per_dim A vector containing the number of items in each 
#'        dimension; `NULL` by default. If the user provides a value for n_dim 
#'        that is \eqn{> 1} but leaves \code{n_i_per_dim = NULL}, assumes that 
#'        the subscales have an equal number of items. 
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
#' redistribute_weights(multi_eq_w, n_dim = 3, n_i_per_dim = c(3, 3, 3), del_i = 2)
#' sum(multi_eq_w)==sum(redistribute_weights(multi_eq_w, n_dim = 3, del_i = 2))
#' 
#' multi_uneq_w <- c(1:12)
#' redistribute_weights(multi_uneq_w, n_dim = 3, n_i_per_dim = c(3, 6, 3), del_i=2)
#' sum(multi_uneq_w)==sum(redistribute_weights(multi_uneq_w, n_dim = 3, 
#'                                             n_i_per_dim = c(3, 6, 3), del_i=2))
#' error_ex <- c(1:12)
#' redistribute_weights(error_ex, n_dim = -3, n_i_per_dim = c(3, 6, 3), del_i = 2)
#' redistribute_weights(error_ex, n_dim = 5, del_i = 2)

redistribute_weights <- function(weights_item, n_dim = 1, n_i_per_dim = NULL,
                               del_i){
  n_items <- length(weights_item)
  new_w <- weights_item; new_w[del_i] <- 0
  del_weight <- weights_item[del_i] #the weight to be redistributed
   
  # Unidimensional
  if ((n_dim == 1) & (is.null(n_i_per_dim) | length(n_i_per_dim) == 1)) {
    new_w[new_w != 0] <- new_w[new_w != 0] + del_weight/length(new_w[new_w != 0])
    # Multidimensional, equal n  
  } else if ((n_dim > 1) & is.null(n_i_per_dim) & n_items %% n_dim != 0){
    stop('Please pass a vector of subscale lengths to n_i_per_dim.')
  } else if ((n_dim > 1) & is.null(n_i_per_dim)) { 
    subscale_len <- n_items / n_dim # subscale length
    # Split indices into dimensions
    i_by_dim <- split(1:n_items, cut(seq_along(1:n_items), n_dim, labels = FALSE))
    for(k in 1:n_dim) {
      if(del_i %in% i_by_dim[[k]]) { # If del_i is in dimension k
        # Create temporary vector to store the remaining indices in dimension k
        temp_i <- i_by_dim[[k]][i_by_dim[[k]] != del_i] 
        non0 <- length(temp_i)
        for(j in temp_i) { # Re-weight the remaining indices in the subscale
          new_w[j][new_w[j] != 0] <- new_w[j][new_w[j] != 0] + del_weight/non0
        }
      } 
    }
    # Multidimensional, unequal n
  } else if ((n_dim > 1) & !is.null(n_i_per_dim)) { 
    # Split indices into dimensions
    i_by_dim <- split(1:n_items, cut(seq_along(1:n_items), 
                                     breaks = cumsum(c(0, n_i_per_dim)), 
                                     labels = FALSE))
    for(k in 1:n_dim) {
      if(del_i %in% i_by_dim[[k]]){ # If del_i is in dimension k
        # Determine number of nonzero item weights in dimension k
        non0 <- length(new_w[i_by_dim[[k]]][new_w[i_by_dim[[k]]] != 0])
        # Create temporary vector to store the remaining indices in dimension k
        temp_i <- i_by_dim[[k]][i_by_dim[[k]] != del_i] 
        for(j in temp_i) { # Re-weight the remaining indices in the subscale
          new_w[j][new_w[j] != 0] <- new_w[j][new_w[j] != 0] + del_weight/non0
        }
      }
    }
  } else {
    stop('Check n_dim and n_i_per_dim')
  }
  return(new_w)
}


#' @title 
#' Compute Cohen's h effect size for the difference in two proportions
#' 
#' @name 
#' cohens_h
#' 
#' @description 
#' \code{cohens_h} Computes Cohen's h (Cohen, 1988) for the difference in two 
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
#' \code{delta_h} Uses the formula below to compute the effect size for impact 
#' of item bias by comparing Cohen's h values for CAI when an item is deleted vs.
#' CAI when all items are included.
#' 
#' e.g. for the improvement in SE:
#' 
#' \eqn{\Delta h^{|k}(SE)=\text{sign}\left(|h({SE)}|-|h^{|k}(SE)|\right)||h^{|k}(SE)|-|h(SE)||}
#' @param h_R h effect sizes for when the item is included.
#' @param h_i_del h effect sizes for when the item is deleted.
#' @return Cohen's h for the difference in the selection accuracy index when the
#'         item is deleted.
#' @examples
#' delta_h(0.04, 0.01)
#' delta_h(-0.002, 0.011)

delta_h <- function(h_R, h_i_del) {
  sign(abs(h_R) - abs(h_i_del)) * abs(abs(h_i_del) - abs(h_R))
}


#' @title 
#' Classification accuracy indices for the reference and focal groups, and 
#' Cohen's h for the difference under strict vs. partial invariance for each group.
#' 
#' @name 
#' acc_indices_h
#' 
#' @description 
#' \code{acc_indices_h} Takes in outputs from [PartInv()] 
#' and returns two restructured data frames with the selection accuracy indices  
#' for the reference and focal groups under the strict invariance and partial
#' invariance conditions, and the corresponding h for the difference in the 
#' selection accuracy indices between these two conditions for each group.

#' @param strict_output [PartInv()]  output (a list) under strict 
#'        invariance. 
#' @param partial_output [PartInv()] output (a list) under partial
#'        invariance.
#' @return A 8 x 3 dataframe with columns `strict invariance`, 
#'        `partial invariance`, and `h`.
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
#' @param weights Vector of item weights. If an item weight is assigned to be 0, 
#' it won't be returned as a biased item.
#' 
#' 
#' @return A vector containing the indices of the biased items.
       
determine_biased_items <- function(lambda_r, lambda_f, nu_r, nu_f, 
                                   Theta_r, Theta_f, weights) {
  biased_lambda <- biased_theta <- biased_nu <- c()
  
  # Compare factor loadings 
  if(is.matrix(lambda_r)) {
    for(i in seq_len(ncol(lambda_r))) {
      items <- as.vector(which(lambda_r[,i] != lambda_f[,i]))
      biased_lambda <- c(biased_lambda, items)}
  } else {
    items <- as.vector(which(lambda_r != lambda_f))
    biased_lambda <- c(biased_lambda, items)
  } 
  # Compare uniqueness
  if(is.matrix(Theta_r)) {
    for(i in seq_len(ncol(Theta_r))) {
      biased_theta <- c(biased_theta, 
                        as.vector(which(Theta_r[,i] != Theta_f[,i])))}
    } else {
      biased_theta <- c(biased_theta, as.vector(c(which(Theta_r != Theta_f))))
    }
  
  # Compare intercepts
  if(is.matrix(nu_r)) {
    for(i in seq_len(ncol(nu_r))) {
      biased_nu <- c(biased_nu, as.vector(which(nu_r[,i] != nu_f[,i])))}
    } else {
      biased_nu <- c(biased_nu, as.vector(which(nu_r != nu_f)))
    }
  biased <- unique(c(biased_lambda, biased_theta, biased_nu)) 
  biased <- setdiff(biased, which(weights==0))
  if(length(biased) == 0) { print("Strict invariance holds for all items.") }
  return(sort(biased))
  }
