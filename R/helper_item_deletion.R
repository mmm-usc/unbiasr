#' @title
#' Compute SR, SE, SP weighted by group proportions
#' 
#' @name 
#' get_overall
#' 
#' @description
#' \code{get_overall} computes overall SR, SE, SP under partial or strict 
#' invariance by weighting the TP, TF, TN, FP values for the reference and focal 
#' groups with the group proportions.
#' 
#' @param pmixr Proportion of the reference group.
#' @param store_summary The summary table from [PartInv()] 
#' under partial or strict invariance.
#' 
#' @return A vector of length 3. 
#'          \item{SR}{Success ratio, computed as \eqn{TP/(TP + FP)}.}
#'          \item{SE}{Sensitivity, computed as \eqn{TP/(TP + FN)}.}
#'          \item{SP}{Specificity, computed as \eqn{TN/(TN + FP)}.}
#'        

get_overall <- function(pmixr, store_summary) {
  r <- store_summary$Reference; f <- store_summary$Focal
  
  SR <- (pmixr*r[1] + (1 - pmixr)*f[1]) /
    (pmixr*r[1] + (1-pmixr)*f[1] + pmixr*r[2] + (1 - pmixr)*f[2]) 
  SE <- (pmixr*r[1] + (1 - pmixr)*f[1]) / 
    (pmixr*r[1] + (1-pmixr)*f[1] + pmixr*r[4] + (1 - pmixr)*f[4])
  SP <- (pmixr*r[3] + (1 - pmixr)*f[3]) / 
    (pmixr*r[3] + (1 - pmixr)*f[3] + pmixr*r[2] + (1 - pmixr)*f[2]) 
  
  return(c(SR, SE, SP))
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
#' of item bias by comparing Cohen's h values for a given selection accuracy 
#' index when an item is deleted vs. included.
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
#' Selection accuracy indices for the reference group, and Cohen's h for their
#' difference under strict vs. partial invariance.
#' 
#' @name 
#' ref_acc_indices_h
#' 
#' @description 
#' \code{ref_acc_indices_h} Takes in outputs from [PartInv()] 
#' and returns a restructured data frame with the selection accuracy indices  
#' for the reference group under the strict invariance and partial invariance 
#' conditions, and the corresponding h for the difference in the selection
#' accuracy indices between these two conditions.

#' @param strict_output [PartInv()]  output (a list) under strict 
#'        invariance. 
#' @param partial_output [PartInv()] output (a list) under partial
#'        invariance.
#' @return A 8 x 3 dataframe with columns `strict invariance`, 
#'        `partial invariance`, and `h`.

ref_acc_indices_h <- function(strict_output, partial_output) {
  ref_par_strict <- partial_output$summary[1][, 1]
  ref_strict <- strict_output$summary[1][, 1]
  r_names <- c("A (true positive)", "B (false positive)", "C (true negative)", 
               "D (false negative)", "Proportion selected", "Success ratio", 
               "Sensitivity", "Specificity")
  df <- data.frame(strict_invariance =  ref_strict, 
                   partial_invariance = ref_par_strict, row.names = r_names)
  df["h"] <- round(cohens_h(df$strict_invariance, df$partial_invariance), 3)
  
  return(df)
}

#' @title 
#' Selection accuracy indices for the reference and focal groups, and Cohen's h
#' for the difference under strict vs. partial invariance for each group.
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
    



