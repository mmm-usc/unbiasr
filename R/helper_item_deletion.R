#' @title
#' Compute PS, SR, SE, SP weighted by group proportions
#' 
#' @name 
#' get_composite_CAI
#' 
#' @description
#' \code{get_composite_CAI} computes composite PS, SR, SE, SP under partial or 
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

get_composite_CAI <- function(pmixr, store_summary) {
  r <- store_summary$Reference; f <- store_summary$Focal
  PS <-  (pmixr*r[1] + (1 - pmixr)*f[1]) + (pmixr*r[2] + (1 - pmixr)*f[2])
  SR <- (pmixr*r[1] + (1 - pmixr)*f[1]) /
    (pmixr*r[1] + (1-pmixr)*f[1] + pmixr*r[2] + (1 - pmixr)*f[2]) 
  SE <- (pmixr*r[1] + (1 - pmixr)*f[1]) / 
    (pmixr*r[1] + (1-pmixr)*f[1] + pmixr*r[4] + (1 - pmixr)*f[4])
  SP <- (pmixr*r[3] + (1 - pmixr)*f[3]) / 
    (pmixr*r[3] + (1 - pmixr)*f[3] + pmixr*r[2] + (1 - pmixr)*f[2]) 
  
  return(c(PS, SR, SE, SP))
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
#' @return `take_one_out` Weights vector with redistributed weights.
#' @examples
#' one_dim_weights <- c(1:7)
#' redistribute_weights(one_dim_weights, del_i = 2)
#' one_dim_weights2 <- c(1:7)
#' redistribute_weights(one_dim_weights2, n_dim = 1, n_i_per_dim = 7, del_i = 2)
#' multi_equal_len_weights <- c(1:9)
#' redistribute_weights(multi_equal_len_weights, n_dim = 3, del_i = 2)
#' multi_equal_len_weights2 <- c(1:9)
#' redistribute_weights(multi_equal_len_weights2, n_dim = 3, 
#'                    n_i_per_dim = c(3, 3, 3), del_i = 2)
#' multi_unequal_len_weights <- c(1:12)
#' redistribute_weights(multi_unequal_len_weights, n_dim = 3, 
#'                    n_i_per_dim = c(3, 6, 3), del_i = 2)
#' error_ex <- c(1:12)
#' redistribute_weights(error_ex, n_dim = -3, 
#'                    n_i_per_dim = c(3, 6, 3), del_i = 2)

redistribute_weights <- function(weights_item, n_dim = 1, n_i_per_dim = NULL,
                               del_i){
  n_items <- length(weights_item)
  take_one_out <- weights_item; take_one_out[del_i] <- 0
  
  # Unidimensional
  if ((n_dim == 1) & (is.null(n_i_per_dim) | length(n_i_per_dim) == 1)) {
    take_one_out <- take_one_out / (n_items - 1) * n_items
    
    # Multidimensional, equal n  
  } else if ((n_dim > 1) & is.null(n_i_per_dim)) { 
    subscale_len <- n_items / n_dim # subscale length
    # Split indices into dimensions
    i_by_dim <- split(1:n_items, cut(seq_along(1:n_items), n_dim, 
                                     labels = FALSE))
    for(k in 1:n_dim) {
      if(del_i %in% i_by_dim[[k]]) { # If del_i is in dimension k
        # Create temporary vector to store the remaining indices in dimension k
        temp_i <- i_by_dim[[k]][i_by_dim[[k]] != del_i] 
        for(j in temp_i) { # Re-weight the remaining indices in the subscale
          take_one_out[j] <- take_one_out[j] / (subscale_len - 1) * subscale_len
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
        subscale_len <- n_i_per_dim[k]
        # Create temporary vector to store the remaining indices in dimension k
        temp_i <- i_by_dim[[k]][i_by_dim[[k]] != del_i] 
        for(j in temp_i) { # Re-weight the remaining indices in the subscale
          take_one_out[j] <- take_one_out[j] / (subscale_len - 1) * subscale_len
        }
      }
    }
  } else {
    stop('Check n_dim and n_i_per_dim')
  }
  return(take_one_out)
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
  df_ref["h"] <- round(cohens_h(df_ref$SFI, df_ref$PFI), 3)
  
  f_par_strict <- partial_output$summary[2][, 1]
  f_strict <- strict_output$summary[2][, 1]
  df_f <- data.frame(SFI =  f_strict, 
                       PFI = f_par_strict, row.names = r_names)
  df_f["h"] <- round(cohens_h(df_f$SFI, df_f$PFI), 3)
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
#' 
#' @return A vector containing the indices of the biased items.
       
determine_biased_items <- function(lambda_r, lambda_f, nu_r, nu_f, 
                                   Theta_r, Theta_f) {
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
  
  if(length(biased) == 0) { print("Strict invariance holds for all items.") }
  return(sort(biased))
}
