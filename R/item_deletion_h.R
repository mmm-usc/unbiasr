#' @title
#' Impact of deleting biased item(s) on classification accuracy indices (CAI)
#'
#' @name
#' item_deletion_h
#'
#' @description
#' \code{item_deletion_h} computes effect size indices that quantify the impact
#'  of (and changes in the impact of) measurement bias on CAI if an item is
#'  dropped vs. retained.
#'  Comparisons are made between CAI computed for the reference group and
#'  expected CAI computed for the focal group; between CAI computed under strict
#'  factorial invariance (SFI) vs. partial factorial invariance (PFI); and
#'  aggregate CAI computed for item subsets.
#' @param n_dim Number of dimensions, 1 by default.
#' @param n_i_per_dim A vector containing the number of items per dimension;
#'     `NULL` by default. If `n_dim` \eqn{> 1} and \code{n_i_per_dim = NULL},
#'      subscales are assumed to have an equal number of items.
#' @param delete_items A vector; default to `NULL`. If `NULL`, only items
#'     determined to contain bias will be considered for deletion.
#' @param delete_one_cutoff User-specified cutoff to use in delete-one scenarios.
#'     `NULL` by default; if `NULL`, PS on the full item set will be used.
#' @param digits Number of digits for rounding. 3 by default.
#' @param ... Other arguments for \code{\link[graphics]{contour}}.
#' @return `item_deletion_h` returns an object of class `itemdeletion` containing
#'     the following elements.
#'     \item{AI}{A data frame storing Adverse Impact (AI) values under partial
#'      invariance by groups.}
#'     \item{ACAI}{A list storing aggregate PS, SR, SE, SP computed for the full
#'      set of items and item subsets excluding biased or user specified items
#'      under partial invariance (PFI).}
#'     \item{h_acai_p}{A list storing Cohen's h values quantifying the impact of
#'      deleting each item considered in the `ACAI` table.}
#'     \item{h_acai_s_p}{A list storing Cohen's h values quantifying the
#'      discrepancy between ACAI under SFI vs. ACAI under PFI.}
#'     \item{delta_h_acai_s_p}{A list storing delta h values quantifying the
#'      impact of deleting an item on discrepancies in h_acai_s_p.}
#'     \item{h_R_EF}{A list storing Cohen's h values quantifying the discrepancy
#'      between observed CAI for the reference group and the expected CAI
#'      computed for the focal group if it matched the distribution of the
#'      reference group (Efocal) under PFI.}
#'     \item{delta_h_R_EF}{A list storing delta h values quantifying the impact
#'      of deleting an item on discrepancies in h_R_Ef.}
#'     \item{h_s_p}{A list containing Cohen's h values quantifying the
#'     discrepancy between CAI under SFI vs. PFI for item subsets.}
#'     \item{delta_h_s_p}{A list storing delta h values quantifying the impact
#'      of deleting an item on discrepancies in h_s_p.}
#'     \item{PartInv}{A list containing PartInv() outputs under PFI and SFI for
#'      each item deletion scenario.}
#'     \item{items}{A vector with items considered for deletion.}
#'     \item{function_call}{Function call to item_deletion_h().}
#'     \item{digits}{Number of digits utilized for rounding.}
#' @rdname PartInv
#' @examples
#' set.seed(7)
#' # Simulate random data to fit a multigroup CFA, invariance across languages
#' library(lavaan)
#' sim_m <-
#'   "f =~ c(1, .7, 1) * x1 + c(.8, 1.1, 1) * x2 + 1 * x3 + 1 * x4 + 1 * x5
#'    f ~~ c(1, 1.3, 1.5) * f
#'    f ~  c(0, .5, 1) * 1
#'    x1 ~ c(0, .3, 0) * 1
#'    x3 ~ c(.3, 0, -.3) * 1
#'    x1 ~~ c(1, .5, 1) * x1"
#' dat_sim <- simulateData(sim_m, sample.nobs = c(120, 90, 50))
#' dat_sim$group <- ifelse(dat_sim$group == 1, "English",
#'                  ifelse(dat_sim$group == 2, "Japanese",
#'                  ifelse(dat_sim$group == 3, "Swahili", NA)))
#' fit_sim <- lavaan::cfa(model = sim_m, data = dat_sim, group = "group")
#' del <- item_deletion_h(cfa_fit = fit_sim, propsel = .05)
#' del # formatted
#' summary(del) #formatted with additional output
#' del$AI # can access all outputs without rounding or formatting
#' del$ACAI
#' del$PartInv_outputs$`Full item set`$summary
#' # choose Japanese as the reference group and use a cutoff:
#' del <- item_deletion_h(cfa_fit = fit_sim, cut_z = 15, reference = "Japanese")
#' del
#'
#' # Multidimensional example
#' l_mat <- matrix(0, nrow = 5, ncol = 2)
#' l_mat[1:2, 1] <- c(.322, .655); l_mat[3:5, 2] <- c(.398, .745, .543)
#' multi_dim <- item_deletion_h(propsel = .05, n_dim = 5,
#'     weights_item = c(1/4, 1/4, 1/6, 1/6, 1/6),
#'     weights_latent = c(0.5, 0.5), alpha_r = c(0, 0), alpha_f = c(-0.3, 0.1),
#'     psi_r = matrix(c(1, 0.5, 0.5, 1), nrow = 2), lambda_r = l_mat,
#'     nu_r = c(.225, .025, .010, .240, .125),
#'     nu_f = c(.225, -.05, .240, -.025, .125), Theta_r = diag(1, 5),
#'     Theta_f = diag(c(1, .95, .80, .75, 1)), plot_contour = TRUE)
#' print(multi_dim)
#' # Single dimension example
#' single_dim <- item_deletion_h(propsel = .10, weights_item = c(1, 0.9, 1, 1),
#'     weights_latent = 0.9, alpha_r = 0.5, alpha_f = 0, psi_r = 1,
#'     lambda_r = c(.3, .5, .9, .7), nu_r = c(.225, .025, .240, .240),
#'     nu_f = c(.225, -.05, .240, -.025), Theta_r = diag(.96, 4), n_dim = 1,
#'     plot_contour = TRUE)
#' print(single_dim)
#' # Using cfa_fit
#' HS <- HolzingerSwineford1939
#' HS$sex <- as.factor(HS$sex)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data = HS, group = "sex")
#' item_deletion_h(cfa_fit = fit, propsel = .05, plot_contour = TRUE)
#' @export
item_deletion_h <- function(cfa_fit = NULL,
                            propsel = NULL,
                            cut_z = NULL,
                            weights_item = NULL,
                            weights_latent = NULL,
                            alpha = NULL, psi = NULL, lambda = NULL, theta = NULL, nu = NULL,
                            pmix = NULL,
                            pmix_ref = 0.5,
                            plot_contour = TRUE, # not sure this is needed
                            labels = NULL, #c("Reference", "Focal"),
                            n_dim = 1,
                            n_i_per_dim = NULL,
                            delete_items = NULL,
                            delete_one_cutoff = NULL,
                            alpha_r = NULL, alpha_f = alpha_r,
                            psi_r = NULL, psi_f = psi_r,
                            lambda_r = NULL, lambda_f = lambda_r,
                            nu_r = NULL, nu_f = nu_r,
                            Theta_r = NULL, Theta_f = Theta_r, reference = NULL,
                            custom_colors = NULL,
                            quadrantsABCD = TRUE,
                            digits = 3,
                            ...) {
  CAIs <- c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  CAIs_star <- paste0(CAIs, "*")
  # make adjustments for formatting and backward compatibility
  argg <- c(as.list(environment()), list(...))
  pl <- prep_params(argg)

  pmix <- pl$pmix
  n_i <- pl$p
  num_g <- pl$num_g
  labels <- pl$labels

  # determine which set of items will be returned
  if (is.null(delete_items)) { # default: return only the biased items.
      delete_items <- determine_biased_items(pl$lambda, pl$nu, pl$theta)
      delete_items <- setdiff(delete_items, which(pl$weights_item == 0))
  } else {
    if (!all(delete_items == floor(delete_items))) {
      stop("'delete_items' should only contain integers corresponding to item indices.")}
    if (!all(delete_items <= n_i)) {
      stop("'delete_items' cannot take integers > the scale length.")}
  }
  dlabs <- c(paste0("|", delete_items))
  store_str <- store_par <- vector(mode = "list", n_i + 1)
  names(store_str) <- names(store_par) <- c("Full item set", dlabs)
  out_str <- c(paste0(
    c("propsel", "cutpt_xi", "cutpt_z", "summary", "bivar_data", "ai_ratio"),
    "_mi"), "labels", "functioncall")
  
  # Call PartInv with the full item set under partial and strict invariance ###
  store_par[[1]] <- do.call(PartInv, c(pl, list(show_mi_result = TRUE)))
  class(store_par[[1]]) <- "PartInv"
  store_str[[1]] <- store_par[[1]][out_str]
  class(store_str[[1]]) <- "PartInv"
  
  # Perform delete i PartInv for all items to be deleted
  # If no cutoff was provided, set propsel based on PartInv output with all items
  pl_del <- pl
  if (is.null(delete_one_cutoff)) {
    pl_del$propsel <- store_par[[1]]$propsel
    pl_del$cut_z <- NULL
  } else {
    pl_del$cut_z <- delete_one_cutoff
    pl_del$propsel <- NULL
  }
  for (i in seq_along(delete_items) + 1) {
    store_par[[i]] <- partinv_del_i(c(pl_del, list(show_mi_result = TRUE)), delete_items[i - 1])
    class(store_par[[i]]) <- "PartInv"
    store_str[[i]] <- store_par[[i]][out_str]
    class(store_str[[i]]) <- "PartInv"
  }

  acai_p <- acai_s <- create_list_of_mats(
    labels[-1], rn = c("Full", dlabs), cn = CAIs_star
  )
  h_acai_s_p <- create_list_of_mats(
    labels[-1], rn = c("Full", dlabs), cn = paste0("h(", CAIs_star, ")")
  )
  h_R_Ef <- create_list_of_mats(
    labels[-1], rn = c("r_Ef", paste0("r_Ef", dlabs)), cn = paste0("h(", CAIs, ")")
  )
  h_acai_p <- create_list_of_mats(
    labels[-1], rn = dlabs, cn = paste0("h(", CAIs_star, ")")
  )
  delta_h_R_Ef <- create_list_of_mats(
    labels[-1], rn = dlabs, cn = paste0("delta_h(", CAIs, ")")
  )
  delta_h_acai_s_p <- create_list_of_mats(
    labels[-1], rn = dlabs, cn = paste0("\u0394h(", CAIs_star, ")")
  )
  h_s_p <- create_list_of_mats(
    labels, rn = c("Full", dlabs), cn = paste0("h(", CAIs, ")")
  )
  delta_h_s_p <- create_list_of_mats(
    labels, rn = dlabs, cn = paste0("\u0394h(", CAIs, ")")
  )
  AI_ratios <- matrix(ncol = num_g, nrow = n_i + 1,
                      dimnames = list(c("Full", dlabs),
                                      c("(SFI)", paste0(labels[-1]))))

  # h: strict vs. partial invariance (full item set) for all groups
  temp_h <- with(store_par[[1]],
    cohens_h(summary[, rep(1, num_g - 1)],
             summary[, seq_len(num_g - 1) + num_g]))

  # Compute aggregate CAI on the full item set
  temp_p <- get_aggregate_CAI(pmix, store_par[[1]]$summary, inv_cond = "partial")
  temp_s <- get_aggregate_CAI(pmix, store_str[[1]]$summary_mi, inv_cond = "strict")

  for (g in seq_len(num_g - 1)) {
    h_R_Ef[[g]][1, ] <- temp_h[, g]
    acai_p[[g]][1, ] <- temp_p[, g]
    acai_s[[g]][1, ] <- temp_s[, g]
    # h: difference between strict and partial invariance for aggregate CAI
    # (on the first row of the data frames in each element of the two lists)
    h_acai_s_p[[g]][1, ] <- cohens_h(temp_s[, g], temp_p[, g])
  }
  AI_ratios[1, ] <- as.vector(c(1, store_par[[1]]$ai_ratio), mode = "double")

  # # Item deletion scenarios ####
  for (i in seq_along(delete_items) + 1) {
    # Check whether improvements in ACAI may be misleading due pmix
    err_improv_acai(i = i, s_full = store_par[[1]]$summary,
                    s_del1 = store_par[[i]]$summary, num_g = num_g)
    err_improv_acai(i = i, s_full = store_str[[1]]$summary_mi,
                    s_del1 = store_str[[i]]$summary_mi, num_g = num_g)

    # Weight the aggregate SR, SE, SP indices under partial and strict invariance
    temp_p_i <- get_aggregate_CAI(pmix, store_par[[i]]$summary, inv_cond = "partial")
    temp_s_i <- get_aggregate_CAI(pmix, store_str[[i]]$summary_mi, inv_cond = "strict")

    # h: strict vs. partial invariance (delete-one item set) for all groups
    temp_h_i <- cohens_h(store_str[[i]]$summary_mi, store_par[[i]]$summary[1:num_g])

    # h: difference in CAI under partial invariance for the ref group vs. for
    # the expected CAI for the focal group with the full item set
    temp_h_r_ef <- with(store_par[[i]],
      cohens_h(summary[, rep(1, num_g - 1)], summary[, seq_len(num_g - 1) + num_g]))

    # May make computation of delete one indices as a method

    for (g in seq_len(num_g - 1)) {
      acai_p[[g]][i, ] <- temp_p_i[, g]
      acai_s[[g]][i, ] <- temp_s_i[, g]
      # compute cohen's h for the difference between the strict and partial inv. conditions
      # (on the i-th row of the data frames in each element of the two lists)
      h_acai_s_p[[g]][i, ] <- cohens_h(temp_s_i[, g], temp_p_i[, g])
      # h: change in aggregate CAI when an item is deleted under partial invariance
      h_acai_p[[g]][i - 1, ] <- cohens_h(acai_p[[g]][1, ], acai_p[[g]][i, ])
      delta_h_acai_s_p[[g]][i - 1, ] <- delta_h(h_acai_s_p[[g]][1, ], h_acai_s_p[[g]][i, ])

      h_s_p[[g]][i - 1, ] <- temp_h_i[, g]
      # delta h: comparing CAI under strict vs. partial invariance when item i is
      # deleted (i.e. the change in h_s_p_ref and h_s_p_foc) for all groups
      delta_h_s_p[[g]][i - 1, ] <- delta_h(temp_h_i[, 1], temp_h_i[, g])

      h_R_Ef[[g]][i, ] <- temp_h_r_ef[, g]
      # change in h_R_Ef_del when item i is deleted (under partial invariance)
      delta_h_R_Ef[[g]][i - 1, ] <- delta_h(temp_h_r_ef[, 1], temp_h_r_ef[, g])
    }
    AI_ratios[i, ] <- c(1, as.numeric(store_par[[i]]$ai_ratio))
  }

  out <- list(
    "AI" = AI_ratios,
    "ACAI" = acai_p,
    "h_acai_p" = h_acai_p,
    "h_acai_s_p" = h_acai_s_p,
    "delta_h_acai_s_p" = delta_h_acai_s_p,
    "h_R_Ef" = h_R_Ef,
    "delta_h_R_Ef" = delta_h_R_Ef,
    "h_s_p" = h_s_p,
    "delta_h_s_p" = delta_h_s_p,
    "PartInv_outputs" = store_par,
    "items" = delete_items,
    "function_call" = match.call()
  )

  class(out) <- "itemdeletion"
  return(out)
}

partinv_del_i <- function(x, i) {
  x$weights_item <- redistribute_weights(
    x$weights_item, n_dim = x$n_dim,
    n_i_per_dim = x$n_i_per_dim, del_i = i)

  # Call PartInv with the new weights ####
  do.call(PartInv, x)
}