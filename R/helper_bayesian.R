#' @title
#' Refit the model in the Bayesian framework.
#'
#' @name
#' bayesian_refit
#'
#' @description
#' \code{bayesian_refit} refits the model in the Bayesian framework.
#'
#' @param cfa_fit Fitted lavaan object.
#' @param dataset Dataset for the analyses. Should contain a column with scale sums.
#' @param col_name_scores String variable. The name of the column containing the sum
#'   scores in `dataset`.
#' @param propsel
#' @param cut_z
#' @param labels
#' @param n.chains Number of chains, defaults to 3.
#' @param post_burnin_sample Length of chain after burnin. 1000 by default.
#' @param weights_item 
#' @param pmix
#' @param ci_probs 
#' @return The output will be a list containing the elements:
#'          \item{bcfa_fit}{Fitted blavaan object.}
#'          \item{lvs}{Posterior samples of the latent variable conditioned on data (list, length n.chains).}
#'          \item{zeta_draws}{`lvs` collapsed across chains, probably will be dropped.}
#'          \item{CAI}{List of length (number of groups). Each list element is a
#'          dataframe with (n.chains x post_burnin_sample) rows and 8 columns, 
#'          which contain the CAI.}
#'          \item{summaries}{Data frame containing the CAI medians, means, and CI 
#'          upper and lower bounds for each group, based on `CAI`.}
#'          \item{post_params}{List containing objects related to the posterior 
#'          parameter draws. ptab is a reduced parTable, factorname is a string
#'          containing the factor name as it was passed to the cfa_fit obj. 
#'          groups contains a vector of group labels. nu, alpha, theta, lambda,
#'          psi contain the posterior draws for each of those parameters, with 
#'          length (n.chains x post_burnin_sample).}
#' THE DOCUMENTATION WILL BE UPDATED 
bayesian_refit <- function(cfa_fit, dataset, col_name_scores, propsel = NULL, cut_z = NULL,
                           labels = NULL, n.chains = 3,
                           post_burnin_sample = 1000, weights_item = NULL,# weights_latent = NULL,
                           pmix = NULL,
                           #show_mi_result = TRUE,
                           digits = 3,
                           ci_probs = c(0.025, 0.975)) { #init?
  if (is.null(cfa_fit) || is.null(dataset) || is.null(col_name_scores)) { 
    warning("Only point estimates are returned. Provide a fitted lavaan object, a dataset, and the name of the column containing total scores to obtain Bayesian results.")
    return(invisible(NULL))
  }
  if (!is.null(col_name_scores) & !is.character(col_name_scores)) {
    stop("Specify the sum score column name as a string in `col_name_scores`.")
  }  
    
  group_var <- as.character(cfa_fit@call$group)
  groups <- labels
  
  #### Fit Bayesian CFA ####
  # Enable parallel processing across cores to speed up MCMC
  #future::plan(future::multisession, workers = 3)
  # Increase allowed obj size to prevent errors
  # options(future.globals.maxSize = 2 * 1024^3) 
  # bfit <- blavaan::bcfa(cfa_fit, data = dataset, group = group_var,
  #              group.label = labels, std.lv = TRUE, save.lvs = TRUE,
  #              sample = post_burnin_sample, n.chains = n.chains)
  # temporarily reading in saved file instead of refitting to expedite debugging
  #saveRDS(bfit, here::here('tempdata/simulation_bfit.rds'))
  bfit <- readRDS(here::here('tempdata/simulation_bfit.rds'))
  
  #### lvs-based posterior CAI ####
  
  # Extract the posterior samples of the latent variable conditioned on data
  fit_lvs <- blavInspect(bfit, "lvs")
  # Collapse across chains
  post_zeta <- do.call(rbind, fit_lvs) # dim: number of obs x n.chains*chain length
  # Obtain posterior distributions of CAI for each group
  CAI_g <- posterior_cai_by_group(zeta_draws = post_zeta, 
                                  obs_y = dataset[col_name_scores], 
                                  g_vec = dataset[group_var], 
                                  z_c = cut_z, groups = groups)
  #library(dplyr)
  # DIGIT IS TEMP, SHOULD BE HANDLED USING CLASSES
  CAI_summaries <- summarize_cai_by_group(CAI_g, ci_probs = ci_probs, digits = digits)
  
  
  #### Bayesian analogue of strict and expected focal ####

  post_params <- .extract_post_params_single_factor(
    b_fit = bfit,
    dataset = dataset,
    groups = groups
  )
  
  ## to be cont. here
  
  
  b_out <- list(bcfa_fit = bfit, 
                lvs = fit_lvs, 
                zeta_draws = post_zeta,
                CAI = CAI_g,
                summaries = CAI_summaries, 
                post_params = post_params)
}



# Extract per-draw parameter lists (alpha, psi, lambda, nu, theta) by group
# Assumes unidimensional CFA model. For theta: supports diagonal residual variances only (lhs==rhs),
# returns a diagonal matrix per group (n_items x n_items) because mn_sd_cov expects matrix OK.
.extract_post_params_single_factor <- function(b_fit, dataset, groups) {
  factor_name <- detect_factor_name(b_fit)
  
  draws <- .combine_mcmc_draws(b_fit)
  
  # Subset parTable rows matching draw columns
  ptab <- get_parTable_subset(b_fit)
  
  relabeled_draws <- relabel_draws_cols_from_ptab(draws, ptab)
  
  alpha <- psi <- lambda <- nu <- theta <- vector("list", length(groups))
  names(alpha) <- names(psi) <- names(lambda) <- names(nu) <- names(theta) <- groups
  
  prefixes <- c("alpha", "psi", "nu", "lambda", "theta")
  draws_by_prefix <- setNames(
    lapply(prefixes, function(p) {
      get_draws_by_prefix(
        relabeled_draws, prefix = p, group_regex = group_regex_from_prefix(p))
    }),
    prefixes
  )
  
  alpha_out <- draws_by_prefix[["alpha"]]
  psi_out <- draws_by_prefix[["psi"]]
  nu_out <- draws_by_prefix[["nu"]]
  lambda_out <- draws_by_prefix[["lambda"]]
  theta_out <- draws_by_prefix[["theta"]]
  
  list(
    factor_name = factor_name,  groups = groups, ptab = ptab, 
    alpha = alpha_out, psi = psi_out, lambda = lambda_out, nu = nu_out, 
    theta = theta_out
  )
}

group_regex_from_prefix <- function(prefix) {
  paste0("^", prefix, ".*_g_([^\\.]+).*$")  # capture group suffix after "_g_"
}
get_draws_by_prefix <- function(draws, prefix, group_regex = NULL,
                                group_levels = NULL) {
  cn <- colnames(draws)
  # identify and subset the columns starting with the prefix
  cols <- grep(paste0("^", prefix), cn, value = TRUE)
  sub_draws <- draws[, cols, drop = FALSE]
  
  # Extract group identifiers from column names
  g <- sub(group_regex, "\\1", cols)

  idx <- split(seq_along(cols), as.character(g))
  by_group <- lapply(idx, function(j) sub_draws[, j, drop = FALSE])
  
  list(cols = cols, by_group = by_group, draws = sub_draws)
}

relabel_draws_cols_from_ptab <- function(draws, ptab) {
  map <- setNames(ptab$par_lab, ptab$label)
  cn  <- colnames(draws)
  
  new_cn <- ifelse(cn %in% names(map), unname(map[cn]), cn)
  
  # Identify first occurrence of each name
  keep <- !duplicated(new_cn)
  
  draws <- draws[, keep, drop = FALSE]
  colnames(draws) <- new_cn[keep]
  
  draws
}


.combine_mcmc_draws <- function(b_fit) {
  mcmc_list <- blavaan::blavInspect(b_fit, "mcmc")
  mats <- lapply(mcmc_list, function(x) as.matrix(x))
  out <- do.call(rbind, mats)
  out
}

# Takes in the latent and observed scale scores and cutoffs, builds a confusion
# matrix and returns a vector containing raw and composite CAI
.compute_cai_from_vec <- function(zeta_vector, z_vector, cut_zeta, cut_z) {
  if (length(zeta_vector) != length(z_vector)) stop("Vectors must have same length.")
  tab <- table(
    factor(zeta_vector >= cut_zeta, levels = c(FALSE, TRUE)), # rows
    factor(z_vector >= cut_z, levels = c(FALSE, TRUE))) # cols
  
  total <- sum(tab)
  TP <- tab[2, 2] / total # A
  FP <- tab[1, 2] / total # B
  TN <- tab[1, 1] / total # C
  FN <- tab[2, 1] / total # D
  
  if (any(c(TP,FP,TN,FN) < -1e-12)) stop("Negative cell proportion.")
  if (abs((TP+FP+TN+FN) - 1) > 1e-8) stop("Cells do not sum to 1.")
  
  PS <- (TP + FP) / (TP + FP + TN + FN)
  SE <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
  SR <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  SP <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
  c("TP" = TP, "FP" = FP, "TN" = TN, "FN" = FN,
    "PS" = PS, "SR" = SR, "SE" = SE, "SP" = SP)
}

# Function that iterates over posterior draws of latent scores
# (zeta_draws), computes group-specific CAI indices using draw-specific
# cutpoints, and returns a list containing posterior distributions of CAI for
# each group
posterior_cai_by_group <- function(zeta_draws, obs_y, g_vec, z_c, groups) {
  g_vec_chr <- as.character(unlist(g_vec))
  obs_y_n <- as.numeric(unlist(obs_y))
  ps_obs <- mean(obs_y_n >= z_c)
  
  # create a list containing relevant indices for each group
  idx_by_g <- split(seq_along(g_vec_chr), g_vec_chr)
  # For each sample (row) of draws (length = chain length x num chain),
  # determine the latent cutoff and compute CAI
  per_draw <- lapply(seq_len(nrow(zeta_draws)), function(i) {
    zeta_draw <- zeta_draws[i, ]
    # Draw-specific cutoff point
    cut_zeta <- quantile(zeta_draw, 1 - ps_obs)
    # For each group, compute CAI from zeta_draw
    lapply(idx_by_g, function(idx) {
      .compute_cai_from_vec(zeta_draw[idx], obs_y_n[idx], cut_zeta, z_c)
    })
  })  
  # Posterior CAI by group 
  out <- 
    lapply(groups, function(g) {
      # Pull the CAI result for group g from each posterior draw
      cai_per_draw <- lapply(per_draw, function(x) x[[as.character(g)]])
      # Drop draws where g is missing
      cai_per_draw <- Filter(Negate(is.null), cai_per_draw)
      if (length(cai_per_draw) == 0) return(NULL)
      # Turn each vector into a 1-row data.frame, then stack rows
      do.call(rbind, lapply(cai_per_draw, function(v) as.data.frame(as.list(v))))
    })
  setNames(out, groups)
}

# iterate over each element over the output from posterior_cai_by_group()
# and compute credible intervals 
summarize_cai_by_group <- function(CAI_list, 
                                   CAIs = c( "TP", "FP", "TN", "FN", "PS", "SE", "SR", "SP"),
                                   ci_probs = c(0.025, 0.975), digits = 3, na.rm = TRUE) {
  rows <- list()
  for (g in names(CAI_list)) {
    df <- CAI_list[[g]]
    if (is.null(df) || !is.data.frame(df)) next
    
    for (cai in CAIs) {
      if (!cai %in% names(df)) next
      x <- df[[cai]]
      
      med <- median(x, na.rm = na.rm); mn <- mean(x, na.rm = na.rm)
      qs <- quantile(x, ci_probs, na.rm = na.rm, names = FALSE)
      
      if (all(is.na(c(med, mn, qs)))) {
        med <- mn <- lb <- ub <- NA_real_
      } else {
        lb <- qs[1]; ub <- qs[2]
      }
      rows[[length(rows) + 1]] <- data.frame(
        group = g, CAI = cai, median = med, mn = mn, lb = lb, ub = ub,
        row.names = NULL)
    }
  }
  out <- do.call(rbind, rows) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits))
  
  rownames(out) <- NULL
  out
}

# Fill in the missing labels such that they match the column names from the mcmc object
populate_label_to_match_mcmc_cols <- function(ptab, factor_name) {
  ptab %>%
    mutate(
      label = case_when(
        lhs == factor_name & rhs == factor_name & op == "~~" ~ 
          paste0(factor_name, "~~", factor_name, ".g", group),
        lhs == factor_name & op == "~1"~ paste0(factor_name, "~1.g", group),
        TRUE ~ label))
}

# set parameter type base labels using lhs, op, rhs from the parTable() output
set_base_labels <- function(ptab, factor_name) {
  ptab %>%  mutate(
    base_name = case_when(
      lhs == factor_name & op == "=~" ~ paste0("lambda_", rhs),
      op == "~1" & lhs != factor_name ~ paste0("nu_", lhs),
      op == "~~" & lhs == rhs & lhs != factor_name ~ paste0("theta_", lhs),
      op == "~~" & lhs == factor_name & rhs == factor_name ~ "psi",
      op == "~1" & lhs == factor_name ~ "alpha",
      TRUE ~ paste0(lhs, op, rhs)))
}
# Filter the parTable object to only rows with labels that match the column names 
# of the combined mcmc object
get_rows_matching_mcmc_cols <- function(ptab, mcmc) {
  ptab %>%
    dplyr::filter(label %in% colnames(mcmc))
}

#' @export
get_parTable_subset <- function(b_fit) {
  # Extract individual parameter draws/samples from the MCMC run 
  mcmc <- blavInspect(b_fit, "mcmc") 
  factor_name <- detect_factor_name(b_fit)
  # Obtain parameter table, set readable labels and get rows matching mcmc draws
  lavaan::parTable(b_fit) %>%
    populate_label_to_match_mcmc_cols(factor_name) %>%
    get_rows_matching_mcmc_cols(mcmc[[1]]) %>%
    set_par_label(factor_name) %>% 
  #  dplyr::select(lhs, op, rhs, par_lab, group, label, plabel, prior, start, est, se, psrf) %>%
    dplyr::filter(!is.na(prior) & trimws(prior) != "")
}

detect_factor_name <- function(fit) {
  lv <- tryCatch(lavaan::lavNames(fit, "lv"), error = function(e) character(0))
  
  if (length(lv) == 1) return(lv)
  if (length(lv) > 1) {
    stop("Multiple latent factors detected (", paste(lv, collapse = ", "),
         "). Current implementation assumes a single latent factor.")
  }
  # otherwise, infer from parTable
  ptab <- lavaan::parTable(fit)
  lv2 <- unique(as.character(ptab$lhs[ptab$op == "=~"]))
  lv2 <- lv2[!is.na(lv2)]
  
  if (length(lv2) == 1) return(lv2)
  
  stop("Could not detect a latent factor name.")
}

# drop_duplicated_rows <- function(df) {
#   df[!duplicated(df), ]
# }

# Set a more readable parameter label (nu, alpha...) that has a suffix indicating
# groups that share this label
set_par_label <- function(ptab, factor_name) {
  df <- ptab %>% set_base_labels(factor_name)  %>%
    # boolean: whether the label varies across groups within each base_name
    label_varies_across_g () %>%
    # determine which groups share a label and set final parameter label 
    group_by(base_name, label) %>%
    mutate(label_tag = {  # groups that share this equality label
      gs <- sort(unique(group))
      paste0("g_", paste0(gs, collapse = "."))}) %>%
    ungroup() %>%
    mutate(par_lab =
             if_else(varies_across_g, paste0(base_name, "_", label_tag),
                     base_name))
  # select relevant columns
  df# %>% 
  #dplyr::select(lhs, op, rhs, group, label, plabel, prior, par_lab)
}
label_varies_across_g <- function(ptab) {
  ptab %>% group_by(base_name) %>%
    mutate(varies_across_g = n_distinct(label) > 1) %>%
    ungroup()
}



# function that compares the rhat, ess values to thresholds provided by the user,
# flags any problematic values, prints message and returns df accordingly
flag_problematic <- function(df, rhat_cut, ess_cut) {
  df <- df %>%
    mutate(
      flag_rhat = rhat > rhat_cut | is.na(rhat),
      flag_ess_bulk = ess_bulk < ess_cut | is.na(ess_bulk),
      flag_ess_tail = ess_tail < ess_cut | is.na(ess_tail)) %>%
    mutate(any_flag = flag_rhat | flag_ess_bulk | flag_ess_tail)
  
  n_flagged <- sum(df$any_flag, na.rm = TRUE) 
  message(sprintf("Total parameters: %d. Flagged (rhat/ess): %d", nrow(df), n_flagged))
  if (n_flagged > 0) {
    message("Top flagged parameters:")
    print(head(df %>% filter(any_flag) %>% arrange(desc(rhat), ess_bulk)))
  } else {
    message("No parameters flagged by thresholds (rhat <= ", rhat_cut, ", ess >= ", ess_cut, ").")
  }
  return(df)
}



summarise_draws_by_par <- function(mcmc_obj, lb, ub, rounding) {
  draws <- as_draws_df(mcmc_obj)
  cbind(
    summarise_draws(draws, "mean", "median", "mad", "sd", "mcse_mean"),
    summarise_draws(draws, ~quantile(.x, probs = c(lb, ub)))[,2:3],
    summarise_draws(draws, "rhat", "ess_bulk", "ess_tail")[, 2:4]) %>%  # ESS rank-nomalized algorithm from vehtari 2021
    arrange(variable) %>%
    mutate(across(where(is.numeric), \(x) round(x, rounding))) 
}

# extracts the MCMC chains from a bcfa object, renames the parameter columns using
# readable labels from a parameter table, removes duplicated parameters, and 
# returns the cleaned list of MCMC chains
mcmc_obj_subset_rename <- function(bcfa_obj, partab_b) {
  # extract individual parameter draws/samples from the MCMC run 
  mcmc_obj <- blavInspect(bcfa_obj, "mcmc")
  # match the column names of f1_mcmc to the corresponding readable names in 
  # par_lab, get unique subset and rename
  par_lab_vec <- partab_b$par_lab[match(colnames(mcmc_obj[[1]]), partab_b$label)]
  for (ch in seq_len(length(mcmc_obj))) { # for each chain
    colnames(mcmc_obj[[ch]]) <- par_lab_vec
    mcmc_obj[[ch]] <- mcmc_obj[[ch]][, !duplicated(colnames(mcmc_obj[[ch]]))]
  }
  mcmc_obj
}
# takes in a list of parameter names and the mcmc object and prints stacked
# density and trace plots of the draws for each parameter
print_trace_density_plots_for_par <- function(mcmc_obj, par_names) {
  if(length(par_names) > 0) {
    draws <- as_draws_df(mcmc_obj)
    for (p in par_names) {
      trace_plot <- mcmc_trace(draws, pars = p) + ggtitle(paste("Trace plot"))# for: ", p))
      dens_plot <- mcmc_dens_overlay(draws, pars = p) + ggtitle(paste("Density plot"))# for: ", p))
      # stack vertically
      combined <- (trace_plot / dens_plot) +
        plot_annotation(
          title = paste("MCMC diagnostics for flagged parameter: ", p),
          theme = theme(plot.title = element_text(size = 14, face = "bold")))
      print(combined)
    }
  } else {
    message("No parameter name was provided.")
  }

}


# takes in the partinv output object, extracts the df containing median, mean, 
# and CI bounds of each CAI based on MCMC draws (for each group), pivots 
# such that CAI are in the rows and the descriptives are in the columns; organized
# by group
pivot_summary_by_group <- function(partinv_out) {
  summaries <- partinv_out[[2]]$summaries
  groups <- if (is.factor(summaries$group)) {
    levels(summaries$group)
  } else {
    unique(summaries$group)
  }
  
  # expected column names after pivot_wider
  expected_cols <- with(expand.grid(value = c("median", "mn", "lb", "ub"), 
                                    grp = groups, stringsAsFactors = FALSE),
                        paste(value, grp, sep = "_"))
  df <- summaries  %>%
    pivot_wider(
      id_cols = CAI,
      names_from = group,
      values_from = c(median, mn, lb, ub),
      names_glue = "{.value}_{group}",
      names_sort = FALSE) %>% 
    select(all_of(c("CAI", intersect(expected_cols, colnames(.))))) %>%
    as.data.frame()
  
  rownames(df) <- df$CAI
  df <- df %>% select(-CAI)
  df
}