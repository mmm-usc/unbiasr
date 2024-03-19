auc <- function(SE, SP){
  TPR <- SE
  FPR <- 1 - SP
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}

#SE = TPR, FPR = 1 - SP
roc_PartInv <- function(cfa_fit,
                               pmix, 
                               from = 0.01,
                               to = 0.25,
                               by = 0.01,
                               cutoffs_from = NULL,
                               cutoffs_to = NULL,
                               mod_names = c("par", "str")) {
  
  est <- format_cfa_partinv(cfa_fit, comp = "est")
  
  propsels <- seq(from = from, to = to, by = by)
  use <- "propsels"
  xl <- "Proportion of selection"
  rangeVals <- propsels
  
  if ((is.null(cutoffs_from) && !is.null(cutoffs_to)) ||
      (!is.null(cutoffs_from) && is.null(cutoffs_to))) {
  #  warning("If you would like to plot CAI at different thresholds, provide
  #          parameter values for both `cutoffs_to` and `cutoffs_from`.
   #         CAI were plotted at different proportions of selection by default.")
  }
  
  # if the user provided the max and min cutoff values, update rangeVals with 
  # a range of cutoffs
  if (!is.null(cutoffs_from) && !is.null(cutoffs_to)) {
    cutoffs <- seq(from = cutoffs_from, to = cutoffs_to, by = by)
    rangeVals <- cutoffs
    xl <- "Thresholds" # for the plots later
    use <- "cutoffs"
  }
  n_g <- cfa_fit@Data@ngroups # number of groups
  
  # if the user did not provide labels, or provided the wrong number of labels,
  if (is.null(labels) || (length(labels) != n_g)) {
    labels <- cfa_fit@Data@group.label
    labels <- paste(labels, c("(reference)", rep("(focal)", n_g - 1)))
  }
  cai <- c("Sensitivity", "Specificity")
  ls_mat <- matrix(NA, ncol = length(rangeVals), nrow = n_g,
                   dimnames = list(labels, rangeVals))
  ls_names <- c(t(outer(cai, Y = mod_names, FUN = paste, sep = "_")))
  ls <- rep(list(ls_mat), length(ls_names))
  names(ls) <- ls_names
  
  # if pmix is missing, assume equal mixing proportions
  if (is.null(pmix)) pmix <- as.matrix(c(rep(1 / n_g, n_g)), ncol = n_g)
  pmix <- as.vector(pmix)
  
  ylabs <- ""
  mains <- ""
  
  # call PartInv with each proportion of selection and store CAI in the list of
  # data frames
  for (p in seq_along(rangeVals)) {
    # if the user provided cutoff values
    if (use == "cutoffs") {
      suppressWarnings({
        pinv <- PartInv(cut_z = cutoffs[p],
                        psi = est$psi,
                        lambda = est$lambda,
                        theta = est$theta,
                        alpha = est$alpha,
                        nu = est$nu,
                        pmix = pmix,
                        plot_contour = FALSE,
                        labels = labels,
                        show_mi_result = TRUE)
      })
    }
    # if the user did not provide cutoff values
    if (use == "propsels") {
      suppressWarnings({
        pinv <- PartInv(propsel = propsels[p],
                        psi = est$psi,
                        lambda = est$lambda,
                        theta = est$theta,
                        alpha = est$alpha,
                        nu = est$nu,
                        pmix = pmix,
                        plot_contour = FALSE,
                        labels = labels,
                        show_mi_result = TRUE)
      })
    }  
   
    num_comb <- length(cai) * length(mod_names) + 1 # for specifying the
    # index within ls
    ind <- 1
    
    while (ind < num_comb) {
    for(i in cai) {
        # for each specified invariance condition
        for (j in seq_along(mod_names)) {
          # if the specified invariance condition is partial inv.,
          ls[[ind]][, p] <-
            ifelse(rep(mod_names[j] == "par", n_g),
                  as.numeric(pinv$summary[i,1:n_g]),
                  as.numeric(pinv$summary_mi[i,1:n_g]))
            
        #  ylabs <- c(ylabs, paste0(cai, " (", cai_names[i], ")"))
          
          temp <- ""
          if (mod_names[j] == "par") temp <- "partial invariance"
          if (mod_names[j] == "str") temp <- "strict invariance"
          
          mains <- c(mains, paste0(cai, " under ", temp))
          ind <- ind + 1
        }
    }
  }
  }
  return(ls)
}                         

#' @example
#' library(lavaan)
#' HS <- HolzingerSwineford1939
#' HS$sex <- as.factor(HS$sex)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' 
#' fit <- cfa(HS.model, data = HS, group = "sex")
#' 
#' out <- roc_PartInv(fit, pmix = c(0.5, 0.5))
#' b <- 1 - out$Specificity_par
#' plot(y = out$Sensitivity_par, x = b)
