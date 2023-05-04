#' @importFrom graphics lines
NULL

#' Plot classification accuracy indices at different proportions of selection
#' 
#' \code{plotPropselRange} plots classification accuracy indices at different proportions of selection
#' under partial and strict invariance conditions for a given CFA fit.
#' 
#' @param cfa_fit CFA model output from lavaan.
#' @param from The lowest proportion of selection to consider
#' @param to The largest proportion of selection to consider
#' @param by The increment of the sequence of proportions.
#' @param pmix List of length `g` containing the mixing proportions of each
#'     group (where `g` is the number of groups). If `NULL`, defaults to `1/g` 
#'     for each group (i.e., the populations have equal size).
#' @param labels A character vector with `g` elements to label the reference
#'     and focal groups on the plot, where `g` is the number of groups. If not
#'     provided, groups are labeled automatically as 'Reference' (for the first
#'     group) and 'Focal_1' through 'Focal_(g-1)', where `g` is the number of
#'     groups.
#' @return Eight plots illustrating how proportion selected (PS), success ratio 
#'     (SR), sensitivity (SE), and specificity (SP) change across different 
#'     proportions of selection under partial and strict invariance conditions.
#' @examples
#' \dontrun{
#' HS <- HolzingerSwineford1939
#' HS$sex <- as.factor(HS$sex)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' 
#' fit <- cfa(HS.model, data = HS, group = "sex")
#' 
#' plotPropselRange(fit, pmix = table(HS$sex)/sum(table(HS$sex)))
#' }
#' @export
plotPropselRange <- function(cfa_fit,
                             from = 0.01,
                             to = 0.25,
                             by = 0.01,
                             pmix = NULL,
                             labels = NULL) {
  est <- format_cfa_partinv(cfa_fit)
  
  propsels <- seq(from = from,to = to, by = by)
  
  n_g <- length(est[['alpha']]) # number of groups
  
  # list to store PS, SE, SR, SP at different proportions of selection
  ls <- vector(length = 8, mode = "list")
  ls[["PS_par"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["PS_str"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["SR_par"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["SR_str"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["SE_par"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["SE_str"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["SP_par"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  ls[["SP_str"]] <- as.data.frame(matrix(NA, ncol = length(propsels), nrow = n_g))
  
  # if the user did not provide labels, or provided the wrong number of labels,
  # set the groups to have labels 'Reference', 'Focal_1'... etc.
  if (is.null(labels) || (length(labels) != n_g)) {
    labels <- c("Reference", paste0("Focal_", 1:(n_g - 1)))
  }
  
  # if pmix is missing, assume equal mixing proportions
  if (is.null(pmix)) pmix <- as.matrix(c(rep(1 / n_g, n_g)), ncol = n_g)
  pmix <- as.vector(pmix)
  
  # call PartInv with each proportion of selection and store CAI in the list of
  # data frames
  for(p in seq_along(propsels)) {
    pinv <- PartInv(propsel = propsels[p],
                    psi = est$psi,
                    lambda = est$lambda,
                    Theta = est$Theta,
                    alpha = est$alpha,
                    nu = est$nu,
                    pmix = pmix,
                    plot_contour = FALSE,
                    labels = labels,
                    show_mi_result = TRUE)
    
    ls[["PS_par"]][, p] <- as.numeric(pinv$summary[5, 1:n_g])
    ls[["PS_str"]][, p] <- as.numeric(pinv$summary_mi[5, 1:n_g])
    ls[["SR_par"]][, p] <- as.numeric(pinv$summary[6, 1:n_g])
    ls[["SR_str"]][, p] <- as.numeric(pinv$summary_mi[6, 1:n_g])
    ls[["SE_par"]][, p] <- as.numeric(pinv$summary[7, 1:n_g])
    ls[["SE_str"]][, p] <- as.numeric(pinv$summary_mi[7, 1:n_g])
    ls[["SP_par"]][, p] <- as.numeric(pinv$summary[8, 1:n_g])
    ls[["SP_str"]][, p] <- as.numeric(pinv$summary_mi[8, 1:n_g])
    
  }
  rownames(ls[["PS_par"]]) <- rownames(ls[["PS_str"]]) <-
    rownames(ls[["SR_par"]]) <- rownames(ls[["SR_str"]]) <-
    rownames(ls[["SE_par"]]) <- rownames(ls[["SE_str"]]) <-
    rownames(ls[["SP_par"]]) <- rownames(ls[["SP_str"]]) <- labels
  
  colnames(ls[["PS_par"]]) <- colnames(ls[["PS_str"]]) <-
    colnames(ls[["SR_par"]]) <- colnames(ls[["SR_str"]]) <-
    colnames(ls[["SE_par"]]) <- colnames(ls[["SE_str"]]) <-
    colnames(ls[["SP_par"]]) <- colnames(ls[["SP_str"]]) <- propsels
  
  colorlist <-  c('#000000', '#f58231', '#e6194b', 'lightskyblue',
                  '#ffe119', '#3cb44b', '#4363d8', '#f032e6',
                  '#bcf60c', '#fabebe', '#911eb4', '#46f0f0',
                  '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000',
                  '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080')
                   #https://sashamaps.net/docs/resources/20-colors/

  # Plot PS partial
  plot(propsels, ls[["PS_par"]][1, ],type ="l", ylim = c(0, 1), 
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Proportion Selected (PS)",
       main = "Proportion selected under partial invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["PS_par"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
    }
  legend("topright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  

  # Plot PS strict
  plot(propsels, ls[["PS_str"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Proportion Selected (PS)",
       main = "Proportion selected under strict invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["PS_str"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("topright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
  # Plot SR partial
  plot(propsels, ls[["SR_par"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Success Ratio (SR)",
         main = "Success ratio under partial invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["SR_par"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("bottomright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
  # Plot SR strict
  plot(propsels, ls[["SR_str"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Success Ratio (SR)",
       main = "Success ratio under strict invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["SR_str"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("bottomright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
  # Plot SE partial
  plot(propsels, ls[["SE_par"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Sensitivity (SE))",
       main = "Sensitivity under partial invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["SE_par"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("bottomright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
  # Plot SE strict
  plot(propsels, ls[["SE_str"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Sensitivity (SE)",
       main = "Sensitivity under strict invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["SE_str"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("bottomright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
  # Plot SP partial
  plot(propsels, ls[["SP_par"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Specificity (SP)",
       main = "Specificity under partial invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["SP_par"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("bottomright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
  # Plot SP strict
  plot(propsels, ls[["SP_str"]][1, ],type ="l", ylim = c(0, 1),
       col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
       ylab = "Specificity (SP)",
       main = "Specificity under strict invariance",
       cex = 1.1)
  for(i in seq_len(n_g - 1)){
    lines(propsels, ls[["SP_str"]][i + 1, ], type = "l",
          lwd = 1.5, col = colorlist[i + 1])
  }
  legend("bottomright", legend = labels, col = colorlist[1:n_g], lty = 1,
         lwd = 1.5, cex = 0.8)  
  
}
