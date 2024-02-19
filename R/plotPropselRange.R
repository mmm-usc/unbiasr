#' @importFrom graphics lines
NULL

#' Plot classification accuracy indices at different proportions of selection
#' 
#' \code{plotPropselRange} plots classification accuracy indices at different
#' proportions of selection under partial and strict invariance conditions for 
#' a given CFA fit.
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
#' @param cai_names A vector of strings indicating the classification accuracy
#'     indices of interest. c("PS", "SR", "SE", "SP") by default.
#' @param mod_names A vector of strings indicating the invariance conditions of
#'     interest. c("par", "str") by default.
#' @return Eight plots illustrating how proportion selected (PS), success ratio 
#'     (SR), sensitivity (SE), and specificity (SP) change across different 
#'     proportions of selection under partial and strict invariance conditions.
#' @examples
#' \dontrun{
#' library(lavaan)
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
                             labels = NULL,
                             cai_names = c("PS", "SR", "SE", "SP"),
                             mod_names = c("par", "str")
                             ) {
  
  stopifnot("cai_names can only take the following values: PS, SR, SE, SP." =
              (all(cai_names %in% c("PS", "SR", "SE", "SP"))))
  stopifnot("mod_names can only take the following values: par, str" =
              (all(mod_names %in% c("par", "str"))))
  
  est <- format_cfa_partinv(cfa_fit, comp = "est")
  
  propsels <- seq(from = from, to = to, by = by)
  
  n_g <- cfa_fit@Data@ngroups # number of groups
  
  # if the user did not provide labels, or provided the wrong number of labels,
  if (is.null(labels) || (length(labels) != n_g)) {
    labels <- cfa_fit@Data@group.label
    labels <- paste(labels, c("(reference)", rep("(focal)", n_g - 1)))
  }

  ls_mat <- matrix(NA, ncol = length(propsels), nrow = n_g,
                   dimnames = list(labels, propsels))
  
  ls_names <- c(t(outer(cai_names, Y = mod_names, FUN = paste, sep = "_")))
  ls <- rep(list(ls_mat), length(ls_names))
  names(ls) <- ls_names
  
  # if pmix is missing, assume equal mixing proportions
  if (is.null(pmix)) pmix <- as.matrix(c(rep(1 / n_g, n_g)), ncol = n_g)
  pmix <- as.vector(pmix)
  
  ylabs <- ""
  mains <- ""
  
  # call PartInv with each proportion of selection and store CAI in the list of
  # data frames
  for(p in seq_along(propsels)) {
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
    num_comb <- length(cai_names) * length(mod_names) + 1 # for specifying the
    # index within ls
    ind <- 1
    while(ind < num_comb) {
      # for each specified CAI
      for (i in seq_along(cai_names)) {
        # swap out the acronym of the composite CAI with the full form
        cai <- lab_cai(substr(cai_names[i], 1, 2))  
        # for each specified invariance condition
       for (j in seq_along(mod_names)) {
         # if the specified invariance condition is partial inv.,
         ls[[ind]][, p] <- 
           ifelse(rep(mod_names[j] == "par", n_g),
                  as.numeric(pinv$summary[cai, 1:n_g]),
                  as.numeric(pinv$summary_mi[cai, 1:n_g]))
         
         ylabs <- c(ylabs, paste0(cai, " (", cai_names[i], ")"))
         
         temp <- ""
         if(mod_names[j] == "par") temp <- "partial invariance"
         if(mod_names[j] == "str") temp <- "strict invariance"
         
         mains <- c(mains, paste0(cai, " under " , temp))
         ind <- ind + 1
       }
      }
     
    }
  }
  mains <- mains[-1]
  ylabs <- ylabs[-1]
  
  colorlist <-  c('#000000', '#f58231', '#e6194b', 'lightskyblue',
                  '#ffe119', '#3cb44b', '#4363d8', '#f032e6',
                  '#bcf60c', '#fabebe', '#911eb4', '#46f0f0',
                  '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000',
                  '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080')
                   #https://sashamaps.net/docs/resources/20-colors/

  legends <- c(rep("topright", 2), rep("bottomright", 6))

  # iterate over each CAI & invariance condition of interest and produce plots
  for (l in seq_along(ls_names)) {
    plot(propsels, ls[[ls_names[l]]][1, ], type = "l", ylim = c(0, 1),
         col = colorlist[1], lwd = 1.5, xlab = "Proportion of selection",
         ylab = ylabs[l],
         main = mains[l],
         cex = 1.1)
    for (i in seq_len(n_g - 1)){
      lines(propsels, ls[[ls_names[l]]][i + 1, ], type = "l",
            lwd = 1.5, col = colorlist[i + 1])
    }
    legend(legends[l], legend = labels, col = colorlist[1:n_g], lty = 1,
           lwd = 1.5, cex = 0.8)  
  }  
}
