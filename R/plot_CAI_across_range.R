#' @importFrom graphics matplot
#' @importFrom grDevices dev.off png dev.cur
#' @importFrom grid grid.echo
NULL

#' Plot classification accuracy indices (CAI) and Adverse Impact ratios (AIR)
#' at different proportions of selection or at different cut score values
#'
#' \code{plot_CAI_across_range} plots CAI and AIR across a range of proportions
#' of selection or cut scores under partial and strict invariance conditions
#' for a given `PartInv` object, using the same reference group and parameter
#' specifications.
#'
#' @param x An object of class [`PartInv`] obtained from \code{\link{PartInv}}.
#' @param labels A character vector with `g` elements to label the reference
#'   and focal groups on the plot, where `g` is the number of groups.
#' @param cai_names A vector of strings indicating the classification accuracy
#'   indices of interest. c("PS", "SR", "SE", "SP", "AIR") by default.
#' @param mod_names A vector of strings indicating the invariance conditions of
#'   interest. c("par", "str") by default.
#' @param from The lowest proportion of selection to consider. `0.01` by
#'   default.
#' @param to The largest proportion of selection to consider. `0.25` by
#'   default.
#' @param by The increment of the sequence of proportions. `0.1` by default.
#'   Note that this argument is used with both cutoffs and proportions of
#'   selection, which have different scales.
#' @param cutoffs_from The lowest cut score to consider. `NULL` by default.
#' @param cutoffs_to The largest ctu score to consider. `NULL` by default.
#' @param custom_colors Optional argument for specifying colors. `NULL` by
#'   default.
#' @param add_AIR_threshold_lines Whether horizontal lines at Adverse Impact
#'   ratios of 1 and 0.8 should be plotted. `TRUE` by default.
#' @param add_vertical_threshold_at Adds a vertical line at a specified
#'   threshold value for easier comparison. `NULL` by default.
#' @param plot_only_g Optional argument, vector of strings specifying the
#'   labels of the subset of groups to be plotted. The reference group is
#'   always plotted. Ignored if all elements do not appear in `labels`.
#' @return Eight plots illustrating how proportion selected (PS), success
#'   ratio (SR), sensitivity (SE), and specificity (SP) change across different
#'   proportions of selection under partial and strict invariance conditions.
#' @examples
#' \dontrun{
#' library(lavaan)
#' HS <- HolzingerSwineford1939
#' HS$sex <- as.factor(HS$sex)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data = HS, group = "sex")
#' p1f <- PartInv(fit, propsel = .7, plot_contour = TRUE,
#'                show_mi_result = TRUE, labels = c("Male", "Female"),
#'                reference = "Female")
#' p1m <- PartInv(fit, propsel = .7, plot_contour = TRUE,
#'                show_mi_result = TRUE, labels = c("Male", "Female"))
#' plot_CAI_across_range(x = p1f, by = 0.01)
#' plot_CAI_across_range(x = p1f, cutoffs_from = 35, cutoffs_to = 50)
#' plot_CAI_across_range(x = p1m, cutoffs_from = 35, cutoffs_to = 50)
#' # plot only SR under partial invariance for up to 10% selection.
#' plot_CAI_across_range(x = p1m, from = 0.01, to = 0.10, cai_names = "AIR",
#'                      mod_names = "par", by = .01)
#' plot_CAI_across_range(x = p1m, from = 0.01, to = 0.10, cai_names = "SR",
#'                      custom_colors = c("green", "orange"), by = 0.01,
#'                      add_vertical_threshold_at = c(.06, .08))
#' }
#' @export
plot_CAI_across_range <- function(
  x = NULL,
  labels = NULL,
  cai_names = c("PS", "SR", "SE", "SP", "AIR"),
  mod_names = c("par", "str"),
  from = 0.01,
  to = 0.25,
  by = 0.1,
  cutoffs_from = NULL,
  cutoffs_to = NULL,
  custom_colors = NULL,
  add_AIR_threshold_lines = TRUE,
  add_vertical_threshold_at = NULL,
  plot_only_g = NULL
) {
  # validate inputs and preprocess CAI/condition selection
  x <- validate_PartInv(x)
  prep <- prep_CAI_inputs(x, cai_names, mod_names)
  cai_names <- prep$cai_names
  plotAIRs <- prep$plotAIRs

  # extract model parameters and group information, used when rerunning PartInv
  pl <- c(x$params, propsel = list(x$propsel), cut_z = list(x$cutpt_z))
  num_g <- pl$num_g
  if (is.null(labels)) {
    labels <- pl$labels
  }
  labels_all <- labels # make copy for AIR indexing

  # validate range parameters
  validate_range_params(from, to, by, cutoffs_from, cutoffs_to)

  # determine plotting range (propsel or cutoff)
  xl <- "Proportion of selection"
  use <- "propsels"
  if (!is.null(cutoffs_from) && !is.null(cutoffs_to)) {
    from <- cutoffs_from
    to <- cutoffs_to
    xl <- "Cut score"
    use <- "cutoffs"
  }
  rangeVals <- seq(from, to, by)

  res <- compute_CAI_and_AIR(
    pl = pl,
    use = use,
    rangeVals = rangeVals,
    cai_names = cai_names,
    mod_names = mod_names,
    num_g = num_g,
    labels = labels
  )

  AIRs <- res$AIRs
  ls <- res$ls

  # subset groups for plotting
  grp <- resolve_group_indices(labels, plot_only_g)
  labels <- grp$labels
  ind <- grp$ind
  num_g <- length(labels)

  colorlist <- set_colors(custom_colors, num_g)

  # plot CAI panels (PS, SR, SE, SP) for the specified invariance conditions
  if (!is.null(cai_names)) {
    plot_CAI_panels(
      ls = ls,
      xl = xl,
      labels = labels,
      colorlist = colorlist[ind],
      add_vertical_threshold_at = add_vertical_threshold_at
    )
  }
  # plot AIRs if requested
  if (plotAIRs && !(num_g == 1 && labels[1] == labels_all[1])) {
    plot_AI_panel(
      AIRs = AIRs,
      labels = labels,
      colorlist = colorlist[ind],
      add_AIR_threshold_lines = add_AIR_threshold_lines
    )
  }
}


############ HELPER FUNCTIONS ############

# New helper: compute CAIs and AIRs across a sequence of values
compute_CAI_and_AIR <- function(
  pl,
  use,
  rangeVals,
  cai_names,
  mod_names,
  num_g,
  labels
) {
  # initialize storage for CAIs and AIRs
  n_vals <- length(rangeVals)
  ls_df <- expand.grid(
    cai = cai_names,
    g = seq_len(num_g),
    mod = mod_names
  )
  
  cai_matrix <- matrix(NA, nrow = nrow(ls_df), ncol = n_vals)
  
  AIRs <- matrix(NA, ncol = n_vals, nrow = max(0, num_g - 1))

  # call PartInv across the requested range and extract and store CAI, AIR
  # values
  for (p in seq_along(rangeVals)) {
    pinv <- run_PartInv_at_value(pl, use, rangeVals[p])

    cai_matrix[, p] <- unlist(
      extract_CAI_from_PartInv(pinv, cai_names, mod_names)
    )

    if (nrow(AIRs) > 0) {
      # pinv$ai_ratio should have length num_g - 1
      AIRs[, p] <- pinv$ai_ratio
    }
  }
  
  colnames(cai_matrix) <- rangeVals
  ls <- cbind(ls_df, cai_matrix)

  if (nrow(AIRs) > 0) {
    rownames(AIRs) <- labels[-1]
    colnames(AIRs) <- rangeVals
  } else {
    colnames(AIRs) <- rangeVals
  }

  list(AIRs = AIRs, ls = ls)
}

plot_CAI_panels <- function(
  ls,
  xl,
  labels,
  colorlist,
  add_vertical_threshold_at = NULL
) {
  rangeVals <- as.numeric(colnames(ls)[-(1:3)])
  for (cc in unique(ls$cai)) {
    for (mm in unique(ls$mod)) {
      mat <- ls[which(ls$cai == cc & ls$mod == mm), -(1:3), drop = FALSE]
      ylab <- paste0(lab_cai(cc), " (", toupper(cc), ")")
      main <- paste0(lab_cai(cc), " under ", lab_mod(mm))
      matplot(rangeVals, t(mat), type = "l", ylim = c(0, 1),
              col = colorlist, lwd = 1.5, xlab = xl,
              ylab = ylab, main = main, cex = 1.1)

      if (!is.null(add_vertical_threshold_at)) {
        abline(v = add_vertical_threshold_at, col = "gray", lty = 3)
      }

      legend(
        make_legend_positions(paste0(cc, "_", mm)),
        legend = labels,
        col = colorlist,
        lty = 1,
        lwd = 1.5,
        cex = 0.8
      )
    }
  }
}

plot_AI_panel <- function(
  AIRs,
  labels,
  colorlist,
  add_AIR_threshold_lines = TRUE
) {
  rangeVals <- as.numeric(colnames(AIRs))
  ylim_u <- ifelse(
    max(AIRs, na.rm = TRUE) < 1.5,
    1.5,
    round(max(AIRs, na.rm = TRUE))
  )
  l_lab <- labels[-1]
  l_col <- colorlist[-1]
  l_lty <- rep(1, length(l_lab))
  l_lwd <- rep(1.5, length(l_lab))

  matplot(
    rangeVals,
    t(AIRs),
    type = "l",
    xlab = "Proportion of selection",
    ylab = "Adverse Impact Ratio (AIR)",
    main = paste0("Adverse Impact Ratios [reference: ", labels[1], "]"),
    ylim = c(0, ylim_u),
    cex.main = 1.1,
    cex.lab = 1.1,
    col = l_col
  )

  if (add_AIR_threshold_lines) {
    abline(h = 1, lty = 2, col = "lightgray", lwd = 0.8)
    abline(h = 0.8, lty = 2, col = "gray42", lwd = 0.8)
    l_lab <- c(l_lab, "AIR = 1", "AIR = 0.8")
    l_col <- c(l_col, "lightgray", "gray42")
    l_lty <- c(l_lty, 2, 2)
    l_lwd <- c(l_lwd, 0.8, 0.8)
  }

  legend("bottomright", l_lab, col = l_col, lty = l_lty, lwd = l_lwd,
         cex = 0.8)
}

prep_CAI_inputs <- function(x, cai_names, mod_names) {
  validate_inputs(x, cai_names, mod_names)

  plotAIRs <- "AIR" %in% cai_names
  cai_names <- setdiff(cai_names, "AIR")
  if (length(cai_names) == 0) {
    cai_names <- NULL
  }

  list(cai_names = cai_names, plotAIRs = plotAIRs)
}

validate_inputs <- function(x, cai_names, mod_names) {
  if (!inherits(x, "PartInv")) {
    stop("`x` must be a PartInv object.")
  }
  if (!all(cai_names %in% c("PS", "SR", "SE", "SP", "AIR"))) {
    stop("`cai_names` must be one or more of: PS, SR, SE, SP, AIR.")
  }
  if (!all(mod_names %in% c("par", "str"))) {
    stop("`mod_names` can only be 'par' or 'str'.")
  }
}

validate_range_params <- function(from, to, by, cutoffs_from, cutoffs_to) {
  # Validate proportion-based range
  if (!is.null(from) && !is.null(to)) {
    if (!is.numeric(from) || !is.numeric(to) || !is.numeric(by)) {
      stop("from, to, and by must be numeric")
    }
    if (from < 0 || to < 0 || from > 1 || to > 1) {
      stop("from and to must be between 0 and 1 for proportions")
    }
    if (from >= to) {
      stop("from must be less than to")
    }
  }

  # Validate cutoff-based range
  if (!is.null(cutoffs_from) && !is.null(cutoffs_to)) {
    if (!is.numeric(cutoffs_from) || !is.numeric(cutoffs_to)) {
      stop("cutoffs_from and cutoffs_to must be numeric")
    }
    if (cutoffs_from >= cutoffs_to) {
      stop("cutoffs_from must be less than cutoffs_to")
    }
  }
  
  if (by <= 0) {
    stop("by must be positive")
  }
}

run_PartInv_at_value <- function(pl, use, value) {
  args <- pl
  args$show_mi_result <- TRUE
  args$functioncall <- NULL

  if (use == "cutoffs") {
    args$cut_z <- value
    args$propsel <- NULL
  } else {
    args$propsel <- value
    args$cut_z <- NULL
  }
  do.call(PartInv, args)
}

extract_CAI_from_PartInv <- function(pinv, cai_names, mod_names) {
  lapply(mod_names, function(mod) {
    summ <- if (mod == "par") pinv$summary else pinv$summary_mi
    out <- summ[lab_cai(cai_names), seq_len(pinv$params$num_g), drop = FALSE]
    as.vector(out)
  })
}

make_legend_positions <- function(ls_names) {
  ifelse(grepl("^PS_|^SR_", ls_names), "topright", "bottomright")
}

set_colors <- function(custom_colors, num_g) {
  if (is.null(custom_colors)) {
    custom_colors <- colorlist()
  } else {
    if (length(custom_colors) > num_g) {
      warning(
        "Length of `custom_colors` > number of groups. Only the first ",
        "`num_g` colors will be used."
      )
    } else {
      if (length(custom_colors) < num_g) {
        warning(
          "`custom_colors` must have length == number of groups. ",
          "Using default colors instead."
        )
        custom_colors <- colorlist()
      }
    }
  }
  custom_colors[seq_len(num_g)]
}

resolve_group_indices <- function(labels, plot_only_g) {
  labels_all <- labels
  ind <- seq_along(labels)

  if (!is.null(plot_only_g) && all(plot_only_g %in% labels)) {
    labels <- unique(c(labels[1], plot_only_g))
    ind <- which(labels_all %in% labels)
  }
  # Handle edge case where all groups are filtered out:
  if (length(ind) == 0) {
    stop("No groups to plot after filtering")
  }
  list(labels = labels, ind = ind)
}

fill_CAI_matrices <- function(ls, vals, p) {
  for (k in seq_along(vals)) {
    ls[[k]][, p] <- vals[[k]]
  }
  ls
}

save_current_plot <- function(
  base_name,
  plot_folder = ".", # Set default in signature
  suffix = "",
  width = 1600,
  height = 1200,
  res = 200
) {
  if (!dir.exists(plot_folder)) {
    dir.create(plot_folder, recursive = TRUE)
  }
  
  fname <- paste0(base_name, if (nzchar(suffix)) paste0("_", suffix), ".png")
  fpath <- file.path(plot_folder, fname)
  
  # More robust approach
  tryCatch({
    grDevices::png(filename = fpath, width = width, height = height, res = res)
    grid::grid.echo()  # or replayPlot() if plot was recorded
    dev.off()
    message("Saved plot to: ", fpath)
  }, error = function(e) {
    if (dev.cur() > 1) dev.off()  # Clean up on error
    warning("Failed to save plot: ", e$message)
  })
}
