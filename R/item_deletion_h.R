#' @title
#' Impact of deleting biased item(s) on classification accuracy indices (CAI)
#'
#' @name
#' item_deletion_h
#'
#' @description
#' \code{item_deletion_h} computes effect size indices that quantify the impact
#'   of (and changes in the impact of) measurement bias on CAI if an item is
#'   dropped vs. retained.
#'   Comparisons are made between CAI computed for the reference group and
#'   expected CAI computed for the focal group; between CAI computed under
#'   strict factorial invariance (SFI) vs. partial factorial invariance (PFI);
#'   and aggregate CAI computed for item subsets.
#' @param x An object of class [`PartInv`] obtained from \code{\link{PartInv}}.
#' @param item_which_dim A vector indicating the dimension to which each item
#'   belongs; `NULL` by default. Only needed for multidimensional scales and
#'   when `reweigh_by_dim = TRUE`.
#' @param reweigh_by_dim Logical indicating whether to redistribute item
#'   weight of the deleted item within the dimension to which it belongs.
#'   Defaults to `TRUE` when `item_which_dim` is provided.
#' @param delete_items A vector; default to `NULL`. If `NULL`, only items
#'   determined to contain bias will be considered for deletion.
#' @param delete_one_cutoff User-specified cutoff to use in delete-one
#'   scenarios. `NULL` by default; if `NULL`, PS on the full item set will
#'   be used.
#' @param update_latent_weights Logical; if `TRUE`, latent weights will be
#'   updated so that the dimension for the deleted item will be reduced
#'   proportionally. Default is `FALSE`.
#' @param ... Other arguments for \code{\link[graphics]{contour}}.
#' @param cfa_fit,propsel,cut_z,weights_item,weights_latent,alpha,psi,lambda,theta,nu,pmix,pmix_ref,plot_contour,labels,n_dim,n_i_per_dim,alpha_r,alpha_f,psi_r,psi_f,lambda_r,lambda_f,nu_r,nu_f,Theta_r,Theta_f,reference,custom_colors,digits,quadrantsABCD
#'     Deprecated; included for backward compatibility. With two groups, '_r' 
#'     and '_f' suffixes refer to the reference group and the focal group.
#' @return `item_deletion_h` returns an object of class `itemdeletion`
#'     containing the following elements.
#'     \item{AI}{A data frame storing Adverse Impact (AI) values under partial
#'      invariance by groups.}
#'     \item{ACAI}{A list storing aggregate PS, SR, SE, SP computed for the
#'      full set of items and item subsets excluding biased or user specified
#'      items under partial invariance (PFI).}
#'     \item{h_acai_p}{A list storing Cohen's h values quantifying the impact
#'      of deleting each item considered in the `ACAI` table.}
#'     \item{h_acai_s_p}{A list storing Cohen's h values quantifying the
#'      discrepancy between ACAI under SFI vs. ACAI under PFI.}
#'     \item{delta_h_acai_s_p}{A list storing delta h values quantifying the
#'      impact of deleting an item on discrepancies in h_acai_s_p.}
#'     \item{h_R_EF}{A list storing Cohen's h values quantifying the
#'      discrepancy between observed CAI for the reference group and the
#'      expected CAI computed for the focal group if it matched the
#'      distribution of the reference group (Efocal) under PFI.}
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
#' partinv_sim <- PartInv(cfa_fit = fit_sim, propsel = .05)
#' del <- item_deletion_h(partinv_sim)
#' del # formatted
#' print(del, full_result = TRUE) #formatted with additional output
#' del$AI # can access all outputs without rounding or formatting
#' del$ACAI
#' del$delete_one_outputs$`|1`$summary
#' # choose Japanese as the reference group and use a cutoff:
#' partinv_sim <- PartInv(cfa_fit = fit_sim, cut_z = 15, reference = "Japanese")
#' del <- item_deletion_h(partinv_sim)
#' del
#'
#' # Multidimensional example
#' l_mat <- matrix(0, nrow = 5, ncol = 2)
#' l_mat[1:2, 1] <- c(.322, .655); l_mat[3:5, 2] <- c(.398, .745, .543)
#' multi_dim_partinv <- PartInv(propsel = .05,
#'     weights_item = c(1/4, 1/4, 1/6, 1/6, 1/6),
#'     weights_latent = c(0.5, 0.5), 
#'     alpha = list(c(0, 0), c(-0.3, 0.1)),
#'     psi = list(matrix(c(1, 0.5, 0.5, 1), nrow = 2), 
#'                matrix(c(1, 0.5, 0.5, 1), nrow = 2)), 
#'     lambda = list(l_mat,l_mat),
#'     nu = list(c(.225, .025, .010, .240, .125),
#'               c(.225, -.05, .240, -.025, .125)), 
#'     theta = list(diag(1, 5), diag(c(1, .95, .80, .75, 1))))
#' item_deletion_h(multi_dim_partinv, item_which_dim = rep(1:2, each = 5))
#' # Single dimension example
#' partinv_single_dim <- PartInv(propsel = .10,
#'     weights_item = c(1, 0.9, 1, 1),
#'     weights_latent = 0.9, alpha = list(0.5, 0), psi = list(1, 1),
#'     lambda = list(c(.3, .5, .9, .7), c(.3, .5, .9, .7)), 
#'     nu = list(c(.225, .025, .240, .240), c(.225, -.05, .240, -.025)), 
#'     theta = list(diag(.96, 4), diag(.96, 4)))
#' item_deletion_h(partinv_single_dim)
#' # Using cfa_fit
#' HS <- HolzingerSwineford1939
#' HS$sex <- as.factor(HS$sex)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data = HS, group = "sex")
#' partinv_fit <- PartInv(cfa_fit = fit, propsel = .05)
#' item_deletion_h(partinv_fit)
#' @export
item_deletion_h <- function(x = NULL,
                            item_which_dim = NULL,
                            reweigh_by_dim = !is.null(item_which_dim),
                            delete_items = NULL,
                            delete_one_cutoff = NULL,
                            update_latent_weights = FALSE,
                            ...,
                            ## legacy args (backward compatibility)
                            cfa_fit = NULL, propsel = NULL, cut_z = NULL,
                            weights_item = NULL, weights_latent = NULL,
                            alpha = NULL, psi = NULL, lambda = NULL, theta = NULL, nu = NULL,
                            pmix = NULL, pmix_ref = 0.5, plot_contour = FALSE, labels = NULL,
                            n_dim = 1, n_i_per_dim = NULL, alpha_r = NULL, alpha_f = alpha_r,
                            psi_r = NULL, psi_f = psi_r, lambda_r = NULL, lambda_f = lambda_r,
                            nu_r = NULL, nu_f = nu_r, Theta_r = NULL, Theta_f = Theta_r,
                            reference = NULL, custom_colors = NULL, quadrantsABCD = TRUE, digits = 3) {
  CAIs <- c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  CAIs_star <- paste0(CAIs, "*")
  # Build a unified argument list from the environment
  argg <- c(as.list(environment()), list(...))

  # make adjustments for formatting and backward compatibility:
  
  #if a PartInv object was not provided, run PartInv() using the args in the env
  if (is.null(x)) {
    # Standardize legacy rf-suffixed params / CFA extraction / weights / pmix / labels
    argg <- prep_params(argg)
    # Create a PartInv object
    x <- do.call(PartInv, c(argg, list(show_mi_result = FALSE)))
    x$params$functioncall <- NULL
    # populate item_which_dim
    argg <- to_item_which_dim(argg)
    x
  }
  
  if (!inherits(x, "PartInv")) {
    stop("`x` must be a PartInv object, or provide legacy parameters so a PartInv can be constructed.")
  }
  # populate item_which_dim (if a PartInv object was provided, make sure 
  # item_which_dim is correctly populated
  argg <- to_item_which_dim(argg, p = length(x$params$nu[[1]]))
  
  x <- add_mi_partinv(x)  # add MI results if not present
  x <- validate_PartInv(x)

  pl <- c(x$params, propsel = list(x$propsel), cut_z = list(x$cutpt_z),
          item_which_dim = list(argg$item_which_dim),
          reweigh_by_dim = list(argg$reweigh_by_dim),
          update_latent_weights = list(update_latent_weights),
          list(...))
  pmix <- pl$pmix
  n_i <- nrow(pl$lambda)
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
  store_del_i <- vector(mode = "list", length(delete_items))
  names(store_del_i) <- dlabs
  # out_str <- c(paste0(
  #   c("propsel", "cutpt_xi", "cutpt_z", "summary", "bivar_data", "ai_ratio"),
  #   "_mi"), "labels")

  # Perform delete i PartInv for all items to be deleted
  # If no cutoff was provided, set propsel based on PartInv output with all items
  pl_del <- pl
  if (is.null(delete_one_cutoff)) {
    pl_del$propsel <- pl$propsel
    pl_del$cut_z <- NULL
  } else {
    pl_del$cut_z <- delete_one_cutoff
    pl_del$propsel <- NULL
  }
 
  for (i in seq_along(delete_items)) {
    store_del_i[[i]] <- partinv_del_i(
      c(pl_del, list(show_mi_result = TRUE)), delete_items[i])
    class(store_del_i[[i]]) <- "PartInv"
  }

  ai_ratios <- lapply(c(list(x), store_del_i), function(x_i) x_i$ai_ratio)

  acai_p <- lapply(c(list(x), store_del_i), extract_cai, key = "a_pi")
  acai_s <- lapply(c(list(x), store_del_i), extract_cai, key = "a_mi")
  rfcai_p <- lapply(c(list(x), store_del_i), extract_cai, key = "rf_pi")
  rfcai_s <- lapply(c(list(x), store_del_i), extract_cai, key = "rf_mi")
  efcai_p <- lapply(c(list(x), store_del_i), extract_cai, key = "Ef_pi")

  # Comparison 1: delete one ACAI vs. full ACAI under PFI
  # h: change in aggregate CAI when an item is deleted under partial invariance
  h_acai_p <- lapply(acai_p[-1], FUN = cohens_h, p1 = acai_p[[1]])
  # Comparison 2: strict vs. partial invariance for ACAI
  # h: difference between strict and partial invariance for aggregate CAI
  h_acai_s_p <- mapply(cohens_h, p1 = acai_s, p2 = acai_p, SIMPLIFY = FALSE)
  # Comparison 3: reference vs. expected focal CAI under PFI
  # h: difference in CAI under partial invariance for the ref group vs. for
  # the expected CAI for the focal groups
  h_R_Ef <- mapply(function(p1, p2) {
    cohens_h(p1[, rep(1, ncol(p2))], p2)
  }, p1 = rfcai_p, p2 = efcai_p, SIMPLIFY = FALSE)
  # Comparison 4: strict vs. partial invariance for CAI (one for each group)
  # h: strict vs. partial invariance for all groups
  h_s_p <- mapply(cohens_h, p1 = rfcai_s, p2 = rfcai_p, SIMPLIFY = FALSE)

  # Delta h computations
  delta_h_acai_s_p <- lapply(h_acai_s_p[-1], FUN = delta_h,
                             h_R = h_acai_s_p[[1]])
  # change in h_R_Ef_del when item i is deleted (under partial invariance)
  delta_h_R_Ef <- lapply(h_R_Ef[-1], FUN = delta_h, h_R = h_R_Ef[[1]])
  # delta h: comparing CAI under strict vs. partial invariance when item i is
  # deleted (i.e. the change in h_s_p_ref and h_s_p_foc) for all groups
  delta_h_s_p <- lapply(h_s_p[-1], FUN = delta_h, h_R = h_s_p[[1]])

  # Format outputs into list of tables
  tbl_ai_ratios <- to_tbl_itemdeletion(
    ai_ratios, rn = c("Full", dlabs), cn = labels[-1])
  tbl_acai_p <- to_tbl_itemdeletion(
    acai_p, rn = c("Full", dlabs), cn = CAIs_star)
  tbl_h_acai_p <- to_tbl_itemdeletion(
    h_acai_p, rn = dlabs, cn = paste0("h(", CAIs_star, ")"))
  tbl_h_acai_s_p <- to_tbl_itemdeletion(
    h_acai_s_p, rn = c("Full", dlabs), cn = paste0("h(", CAIs_star, ")"))
  tbl_delta_h_acai_s_p <- to_tbl_itemdeletion(
    delta_h_acai_s_p, rn = dlabs, cn = paste0("\u0394h(", CAIs_star, ")"))
  tbl_h_R_Ef <- to_tbl_itemdeletion(
    h_R_Ef, rn = c("Full", dlabs), cn = paste0("h(", CAIs, ")"),
    labels = labels[-1])
  tbl_h_s_p <- to_tbl_itemdeletion(
    h_s_p, rn = c("Full", dlabs), cn = paste0("h(", CAIs, ")"),
    labels = labels)
  tbl_delta_h_R_Ef <- to_tbl_itemdeletion(
    delta_h_R_Ef, rn = dlabs, cn = paste0("\u0394h(", CAIs, ")"),
    labels = labels[-1])
  tbl_delta_h_s_p <- to_tbl_itemdeletion(
    delta_h_s_p, rn = dlabs, cn = paste0("\u0394h(", CAIs, ")"),
    labels = labels)

  # HL: Not working properly
  # for (i in seq_along(store_del_i)) {
  #   # Check whether improvements in ACAI may be misleading due pmix
  #   err_improv_acai(i = i, s_full = x$summary,
  #                   s_del1 = store_del_i[[i]]$summary, num_g = num_g)
  #   err_improv_acai(i = i, s_full = x$summary_mi,
  #                   s_del1 = store_del_i[[i]]$summary_mi, num_g = num_g)
  # }

  structure(
    list(
      "AI" = tbl_ai_ratios,
      "ACAI" = tbl_acai_p,
      "h_acai_p" = tbl_h_acai_p,
      "h_acai_s_p" = tbl_h_acai_s_p,
      "delta_h_acai_s_p" = tbl_delta_h_acai_s_p,
      "h_R_Ef" = tbl_h_R_Ef,
      "delta_h_R_Ef" = tbl_delta_h_R_Ef,
      "h_s_p" = tbl_h_s_p,
      "delta_h_s_p" = tbl_delta_h_s_p,
      "delete_one_outputs" = store_del_i,
      "items" = delete_items),
    class = "itemdeletion"
  )
}

partinv_del_i <- function(x, i) {
  # print("in partinv_del_i()")
  x$weights_item <- redistribute_weights2(
    w = x$weights_item, del_i = i, item_which_dim = x$item_which_dim,
    reweigh_by_dim = x$reweigh_by_dim)

  if (x$update_latent_weights) {
    x$weights_latent <- update_lw(
      w = x$weights_latent, del_i = i, item_which_dim = x$item_which_dim)
  }
  # Call PartInv with the new weights ####
  out <- do.call(PartInv, x)
  out$params$functioncall <- NULL
  out
}

to_tbl_itemdeletion <- function(x, rn, cn, labels = NULL) {
  if (!is.null(labels)) {
    out <- lapply(seq_along(labels), function(j) {
      tbl <- do.call(rbind, lapply(x, function(x_i) x_i[, j]))
      dimnames(tbl) <- list(rn, cn)
      tbl
    })
    names(out) <- labels
    return(out)
  } else {
    out <- do.call(rbind, lapply(x, function(x) as.numeric(unlist(x))))
    dimnames(out) <- list(rn, cn)
    return(out)
  }
}