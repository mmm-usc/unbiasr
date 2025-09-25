#' @importFrom graphics lines
#' @importFrom grDevices dev.off png replayPlot recordPlot
NULL

#' Plot classification accuracy indices at different proportions of selection
#' or at different threshold (cutoff) values
#' 
#' \code{plot_CAI_across_range} plots classification accuracy indices at different
#' proportions of selection under partial and strict invariance conditions for 
#' a given CFA fit or set of parameter estimates.
#' 
#' @param cfa_fit CFA model output from lavaan. `NULL` by default.
#' @param pmix List of length `g` containing the mixing proportions of each
#'   group (where `g` is the number of groups). If `NULL`, defaults to `1/g` 
#'   for each group (i.e., the populations have equal size).
#' @param weights_item A vector of item weights.
#' @param weights_latent A vector of latent factor weights.
#' @param alpha A list of length `g` containing `1 x d` latent factor mean
#'   vectors where `g` is the number of groups and `d` is the number of latent
#'   dimensions. The first element is assumed to belong to the reference group.
#' @param psi A list of length `g` containing `d x d` latent factor
#'   variance-covariance matrices where `g` is the number of groups and `d` is
#'   the number of latent dimensions. The first element is assumed to belong
#'   to the reference group.
#' @param lambda A list of length `g` containing `n x d` factor loading matrices
#'     where `g` is the number of groups, `d` is the number of latent dimensions,
#'     and `n` is the number of items. The first element is assumed
#'     to belong to the reference group.
#' @param nu A list of length `g` containing `1 x n` measurement intercept
#'     vectors where `g` is the number of groups and `n` is the number of items.
#'     The first element is assumed to belong to the reference group.
#' @param theta A list of length `g` containing `1 x n` vectors or `n x n`
#'     matrices of unique factor variances and covariances, where `g` is the
#'     number of groups and `n` is the number of items. The first element is
#'     assumed to belong to the reference group.
#' @param labels A character vector with `g` elements to label the reference
#'   and focal groups on the plot, where `g` is the number of groups. If not
#'   provided, groups are labeled automatically as 'Reference' (for the first
#'   group) and 'Focal_1' through 'Focal_(g-1)', where `g` is the number of
#'   groups.
#' @param cai_names A vector of strings indicating the classification accuracy
#'   indices of interest. c("PS", "SR", "SE", "SP", "AI) by default.
#' @param mod_names A vector of strings indicating the invariance conditions of
#'   interest. c("par", "str") by default.
#' @param from The lowest proportion of selection to consider. `0.01` by default.
#' @param to The largest proportion of selection to consider. `0.25` by default.
#' @param by The increment of the sequence of proportions. `0.01` by default.
#' @param cutoffs_from The lowest threshold to consider. `NULL` by default.
#' @param cutoffs_to The largest threshold to consider. `NULL` by default.
#' @return Eight plots illustrating how proportion selected (PS), success ratio 
#'   (SR), sensitivity (SE), and specificity (SP) change across different 
#'   proportions of selection under partial and strict invariance conditions.
#' @param custom_colors Optional argument for specifying colors. `NULL` by default.  
#' @param reference Optional argument for specifying the reference group.
#' @param add_AI_threshold_lines Whether horizontal lines at Adverse Impact 
#' ratios of 1 and 0.8 should be plotted. `TRUE` by default.
#' @param add_vertical_threshold_at Adds a vertical line at a specified threshold
#'   value for easier comparison. `NULL` by default.  
#' @param plot_only_g Optional argument, vector of strings specifying the labels
#'   of the subset of groups to be plotted. The reference group is always 
#'   plotted. Ignored if all elements do not appear in `labels`.
#' @param saveplots Logical; if TRUE, saves plots to files. `FALSE` by default.
#' @param plot_folder Optional folder name for saved plots. Created if missing. 
#'   If no folder name is provided, saves plots in the current working directory.
#' @param suffix Optional string suffix appended to plot filenames. `""` by default.
#' @param ... Additional arguments.
#' @param ... Additional arguments.
#' @examples
#' \dontrun{
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
#' plot_CAI_across_range(cfa_fit = fit_sim)
#' plot_CAI_across_range(cfa_fit = fit_sim, custom_colors = c("blue", "pink", "red"))
#' plot_CAI_across_range(cfa_fit = fit_sim, plot_only_g = "Japanese")
#' plot_CAI_across_range(cfa_fit = fit_sim, add_vertical_threshold_at = 0.07)
#' # use parameter estimates rather than cfa_fit object
#' ests <- unnest_list(lavInspect(fit_sim, "est"))
#' plot_CAI_across_range(alpha = ests$alpha, lambda = ests$lambda, 
#'   psi = ests$psi, theta = ests$theta, nu = ests$nu)
#' library(lavaan)
#' HS <- HolzingerSwineford1939
#' HS$sex <- as.factor(HS$sex)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data = HS, group = "sex")
#' plot_CAI_across_range(cfa_fit = fit, pmix = table(HS$sex) / sum(table(HS$sex)), 
#'                  cutoffs_from = 35, cutoffs_to = 50)
#' # plot only SR under partial invariance for up to 10% selection.
#' plot_CAI_across_range(cfa_fit = fit, pmix = table(HS$sex)/sum(table(HS$sex)), 
#'     from = 0.01, to = 0.10, cai_names = "SR", mod_names = "par", 
#'      labels = c("Male", "Female"))
#' }
#' @export
plot_CAI_across_range <- function(
    cfa_fit = NULL,
    pmix = NULL,
    weights_item = NULL, weights_latent = NULL,
    alpha = NULL, psi = NULL, lambda = NULL, theta = NULL, nu = NULL,
    labels = NULL,
    cai_names = c("PS", "SR", "SE", "SP", "AI"),
    mod_names = c("par", "str"),
    from = 0.01,
    to = 0.25,
    by = 0.01,
    cutoffs_from = NULL,
    cutoffs_to = NULL, 
    custom_colors = NULL, 
    reference = NULL, 
    add_AI_threshold_lines = TRUE, 
    add_vertical_threshold_at = NULL,
    plot_only_g = NULL, 
    saveplots = FALSE,
    plot_folder = NULL,
    suffix = "",
    ...
    ) {
  stopifnot("cai_names can only take the following values: PS, SR, SE, SP, AI." =
              (all(cai_names %in% c("PS", "SR", "SE", "SP", "AI"))))
  stopifnot("mod_names can only take the following values: par, str" =
              (all(mod_names %in% c("par", "str"))))
  plotAIs <- TRUE
  if ("AI" %in% cai_names) {
    cai_names <- cai_names[!cai_names %in% "AI"]
    if (length(cai_names) == 0) {
      cai_names <- NULL
    }
  } else {
    plotAIs <- FALSE
  }
  
  if (saveplots) {
    if (is.null(plot_folder)) plot_folder <- "."  # default to working dir
    if (!dir.exists(plot_folder)) dir.create(plot_folder, recursive = TRUE)
  }
  
  pl <- prep_params(
    cfa_fit = cfa_fit, weights_item = weights_item, 
    weights_latent = weights_latent, 
    alpha = alpha, psi = psi, lambda = lambda, theta = theta, nu = nu, 
    pmix = pmix, labels = labels, reference = reference,
    custom_colors = custom_colors)
  
  alpha <- pl$alpha
  psi <- pl$psi
  lambda <- pl$lambda
  nu <- pl$nu
  theta <- pl$theta
  pmix <- pl$pmix
  num_g <- pl$num_g
  weights_latent <- pl$weights_latent
  weights_item <- pl$weights_item
  labels <- pl$labels
  
  propsels <- seq(from = from, to = to, by = by)
  use <- "propsels"
  xl <- "Proportion of selection"
  rangeVals <- propsels
  
  if ((is.null(cutoffs_from) && !is.null(cutoffs_to)) ||
    (!is.null(cutoffs_from) && is.null(cutoffs_to))) {
    warning("If you would like to plot CAI at different thresholds, provide
            parameter values for both `cutoffs_to` and `cutoffs_from`.
            CAI were plotted at different proportions of selection by default.")
  }
    
  # if the user provided the max and min cutoff values, update rangeVals with 
  # a range of cutoffs
  if (!is.null(cutoffs_from) && !is.null(cutoffs_to)) {
    cutoffs <- seq(from = cutoffs_from, to = cutoffs_to, by = by)
    rangeVals <- cutoffs
    xl <- "Thresholds" # for the plots later
    use <- "cutoffs"
  }

  ls_mat <- matrix(NA, ncol = length(rangeVals), nrow = num_g)
  AIs <- matrix(NA, ncol = length(rangeVals), nrow = num_g - 1)
  ls_names <- c(t(outer(cai_names, Y = mod_names, FUN = paste, sep = "_")))
  ls <- rep(list(ls_mat), length(ls_names))
  names(ls) <- ls_names
  
  ylabs <- ""
  mains <- ""
  
  # call PartInv with each propsel/cutoff and store CAI in the list of dfs
  for (p in seq_along(rangeVals)) {
    # if the user provided cutoff values
    if (use == "cutoffs") {
      suppressWarnings({
        pinv <- PartInv(cut_z = cutoffs[p],
                        psi = psi,
                        lambda = lambda,
                        theta = theta,
                        alpha = alpha,
                        nu = nu,
                        pmix = pmix,
                        plot_contour = FALSE,
                        labels = labels,
                        show_mi_result = TRUE, 
                        reference = reference, 
                        weights_item = weights_item,
                        weights_latent = weights_latent)
      })
    }
    # if the user did not provide cutoff values
    if (use == "propsels") {
      suppressWarnings({
        pinv <- PartInv(propsel = propsels[p],
                        psi = psi,
                        lambda = lambda,
                        theta = theta,
                        alpha = alpha,
                        nu = nu,
                        pmix = pmix,
                        plot_contour = FALSE,
                        labels = labels,
                        show_mi_result = TRUE,
                        reference = reference, 
                        weights_item = weights_item,
                        weights_latent = weights_latent)
      })
    }  
    # for specifying the index within ls
    num_comb <- length(cai_names) * length(mod_names) + 1 

    ind <- 1

    while (ind < num_comb) {
      # for each specified CAI
      for (i in seq_along(cai_names)) {
        # swap out the acronym of the composite CAI with the full form
        cai <- lab_cai(substr(cai_names[i], 1, 2))
        # for each specified invariance condition
        for (j in seq_along(mod_names)) {
          # if the specified invariance condition is partial inv.,
          ls[[ind]][, p] <-
            ifelse(rep(mod_names[j] == "par", num_g),
              as.numeric(pinv$summary[cai, 1:num_g]),
              as.numeric(pinv$summary_mi[cai, 1:num_g])
            )
          ylabs <- c(ylabs, paste0(cai, " (", cai_names[i], ")"))

          temp <- ""
          if (mod_names[j] == "par") temp <- "partial invariance"
          if (mod_names[j] == "str") temp <- "strict invariance"

          mains <- c(mains, paste0(cai, " under ", temp))
          ind <- ind + 1
        }
      }
    }
    AIs[,p] <- as.numeric(pinv$ai_ratio)
  }

  rownames(AIs) <- labels[-1]
  colnames(AIs) <- rangeVals
 
  ls <- lapply(ls, function(mat) {
    dimnames(mat) <- list(labels, rangeVals)
    mat
  })
  
  mains <- mains[-1]
  ylabs <- ylabs[-1]
  
  colorlist <- colorlist()
  if (!is.null(custom_colors)) { colorlist <- custom_colors }
  ind <- 1:num_g
  
  legends <- c(rep("topright", 2), rep("bottomright", 6))
  
  labels_temp <- labels # duplicate
  # if the user specified a subset of groups to be plotted and the labels match
  if (!is.null(plot_only_g) && all(plot_only_g %in% labels)) {
    labels <- unique(c(labels_temp[1], plot_only_g)) # ensure reference is included
    num_g <- length(labels)
    ind <- which(labels_temp %in% labels)
  }
  #colorlist <- colorlist[ind]
  
  if (!is.null(cai_names)) {
    # iterate over each CAI & invariance condition of interest and produce plots
    for (l in seq_along(ls_names)) {
      l_lab <- labels
      l_col <- colorlist[ind]
      l_lty <- rep(1, num_g)
      
      plot(0, type = "l", ylim = c(0, 1), xlim = c(min(rangeVals), max(rangeVals)),
           col = colorlist[1], lwd = 1.5, xlab = xl,
           ylab = ylabs[l],
           main = mains[l],
           cex = 1.1)
      
      if (!is.null(add_vertical_threshold_at)) {
        abline(v = add_vertical_threshold_at, col = "gray", lty = 3)
      }

      lines(rangeVals, ls[[ls_names[l]]][1, ], type = "l", col = colorlist[1], lwd = 1.5)
      for (i in ind[-1]) {
        lines(rangeVals, ls[[ls_names[l]]][i, ], type = "l",
              lwd = 1.5, col = colorlist[i])
      }
      legend(legends[l], legend = l_lab, col = l_col, lty = l_lty,
             lwd = 1.5, cex = 0.8)  
      
      if (saveplots) {
        fname <- file.path(
          plot_folder,
          paste0(ls_names[l], if (nzchar(suffix)) paste0("_", suffix), ".png")
        )
        p <- recordPlot()
        invisible({
          png(fname, width = 1600, height = 1200, res = 200)
          replayPlot(p)
          dev.off()
        })
      }
      
    }
  }
  # if the user wants AI plots and if the only group specified to be 
  # plotted is not the reference group 
  if (plotAIs && !(num_g == 1 && labels[1]==labels_temp[1])) {
    #colorlist <- colorlist[-1]
    ylim_u <- ifelse(max(AIs) < 1.5, 1.5, round(max(AIs)))
    
    l_lab <- labels[-1]
    l_col <- colorlist[ind][-1]
    l_lty <- rep(1, num_g - 1)
    l_lwd <- rep(1.5, num_g - 1)
    
    plot(0, xlim = c(min(rangeVals), max(rangeVals)), ylim = c(0, ylim_u),
         ylab = "Adverse Impact Ratio (AIR)", cex = 1.1,
         main = paste0("Adverse Impact Ratios [reference group: ", labels[1], "]"))
    if (add_AI_threshold_lines) {
      abline(h = 1, lty = 2, col = "lightgray", lwd = 0.8)
      abline(h = 0.8, lty = 2, col = "gray42", lwd = 0.8)
      l_lab <- c(l_lab, "AIR = 1", "AIR = 0.8")
      l_col <- c(l_col, "lightgray", "gray42")
      l_lty <- c(l_lty, 2, 2)
      l_lwd <- c(l_lwd, 0.8, 0.8)
    }
    for (i in ind[-1]) {
      lines(rangeVals, AIs[labels_temp[i],], type = "l", lwd = 1.5, 
            col = colorlist[i])
      }
    legend("bottomright", legend = l_lab, col = l_col, lty = l_lty, lwd = l_lwd, 
           cex = 0.8)  
    
    if (saveplots) {
      fname <- file.path(
        plot_folder,
        paste0("AI", if (nzchar(suffix)) paste0("_", suffix), ".png")
      )
      p <- recordPlot()  # capture the plot that just appeared on screen
      invisible({
        png(fname, width = 1600, height = 1200, res = 200)
        replayPlot(p) # redraw into png file
        dev.off()
      })
    }
    
  }
}

